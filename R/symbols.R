#' @include utils.R
#' @include benchmark.R
#' @include parser.R
#' @include documentation.R
#' @include parse-tree-utils.R
#' @include symbol-table.R
#' @include symbols-qualified.R
#' @include symbols-s3.R
#' @include symbols-s4.R
#' @include symbols-r6.R
#' @include symbols-shiny.R
NULL

# This script derives symbol definitions and relationships from parsed modules.
# It maps R constructs into SCIP-compatible descriptors and metadata bundles.
# Integrated with symbols-qualified.R for package-qualified symbol tracking.
# Uses symbol-table.R for cross-file reference resolution.

#' Resolve symbols present in an AST module.
#'
#' Uses a package-wide symbol table (when provided) to enable cross-file reference
#' resolution. When a function call references a symbol defined in another file,
#' the occurrence will point to the same global symbol ID as the definition.
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @param workspace Workspace context with metadata and dependencies.
#' @param track_externals Logical; if TRUE, track package-qualified external symbols (default: TRUE).
#' @param symbol_table Optional symbol table from `build_symbol_table()` for cross-file resolution.
#' @return A list containing symbol information, occurrence records, and external references.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' resolve_symbols(ast, analyze_workspace(tempdir()))
#' resolve_symbols(ast, analyze_workspace(tempdir()), track_externals = FALSE)
#' # With symbol table for cross-file resolution:
#' symbol_table <- build_symbol_table(parsed_files, workspace)
#' resolve_symbols(ast, workspace, symbol_table = symbol_table)
#' }
resolve_symbols <- function(ast_module, workspace, track_externals = TRUE, symbol_table = NULL, tree_index = NULL) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list(symbols = list(), occurrences = list(), external_refs = list()))
  }

  file_info <- ast_module$file_info %||% list(relative_path = basename(ast_module$file_info$path))
  relative_path <- file_info$relative_path %||% basename(file_info$path)

  # OPTIMIZATION: Reuse tree index from Phase 2 if available, otherwise build it
  if (is.null(tree_index)) {
    t_idx <- bench_start("build_tree_index", list(row_count = nrow(data)))
    tree_index <- build_tree_index(data)
    bench_end(t_idx)
  } else {
    bench_count("tree_index_reuse")
  }

  # Pre-compute assignment positions for O(1) line lookup
  # Use environment for fast "is there assignment on this line" check
  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  assignment_rows <- data[data$token %in% assignment_tokens, , drop = FALSE]
  assignment_by_line <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_len(nrow(assignment_rows))) {
    line_key <- as.character(assignment_rows$line1[i])
    if (is.null(assignment_by_line[[line_key]])) {
      assignment_by_line[[line_key]] <- list()
    }
    assignment_by_line[[line_key]][[length(assignment_by_line[[line_key]]) + 1]] <- assignment_rows$col1[i]
  }

  symbols <- list()
  occurrences <- list()
  # OPTIMIZATION: Use environment for O(1) seen symbol check instead of %in%
  seen_symbols_env <- new.env(hash = TRUE, parent = emptyenv())

  # Track definition scopes for relationship building
  definition_scopes <- list()

  # LAZY LOCAL SYMBOL CREATION: Create local symbols on-demand
  # Maps position_key -> symbol_id for deduplication
  local_symbol_registry <- new.env(hash = TRUE, parent = emptyenv())
  local_counter <- 0L

  # Get definitions_env from symbol_table for O(1) lookups
  definitions_env <- if (!is.null(symbol_table)) symbol_table$definitions_env else NULL

  # FIX 1: Include SYMBOL_FORMALS to detect function parameters
  symbol_rows <- data[data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"), ]
  symbol_rows <- symbol_rows[order(symbol_rows$line1, symbol_rows$col1), , drop = FALSE]

  n_symbols <- nrow(symbol_rows)
  for (idx in seq_len(n_symbols)) {
    bench_count("symbol_iteration")
    row <- symbol_rows[idx, ]
    name <- row$text
    kind <- determine_symbol_kind_indexed(row, data, tree_index)

    # Determine if this is a definition
    # SYMBOL_FORMALS are always definitions (function parameters)
    if (row$token == "SYMBOL_FORMALS") {
      is_definition <- TRUE
      is_local_scope <- TRUE  # Parameters are always local to their function
    } else {
      # FIX 2: Check for loop variables (forcond parent) - use indexed lookup
      parent_id <- get_parent_id_indexed(row$id, tree_index)
      parent_token <- if (!is.na(parent_id)) {
        row_idx_val <- get_row_indexed(parent_id, tree_index)
        if (!is.na(row_idx_val)) data$token[row_idx_val] else NA
      } else {
        NA
      }
      is_loop_var <- !is.na(parent_token) && parent_token == "forcond"

      # Check for assignment - O(1) line lookup
      line_key <- as.character(row$line1)
      line_assignments <- assignment_by_line[[line_key]]
      on_assignment_line <- if (!is.null(line_assignments)) {
        any(vapply(line_assignments, function(col) col > row$col1, logical(1)))
      } else {
        FALSE
      }

      is_definition <- is_loop_var || (row$token == "SYMBOL" && on_assignment_line)

      # Check if this is inside a function (local scope)
      # OPTIMIZATION: Use O(1) pre-computed lookup instead of O(depth) tree walk
      is_local_scope <- is_loop_var ||
        row$token == "SYMBOL_FORMALS" ||
        is_inside_function_indexed(row$id, tree_index)
    }

    # LAZY LOCAL SYMBOL CREATION: Resolve symbol with environment lookups
    symbol_id <- resolve_symbol_id_fast(
      name = name,
      kind = kind,
      is_definition = is_definition,
      is_local_scope = is_local_scope,
      is_call = (row$token == "SYMBOL_FUNCTION_CALL"),
      relative_path = relative_path,
      definitions_env = definitions_env,
      local_registry = local_symbol_registry,
      local_counter_env = environment()  # For updating local_counter
    )

    # For definitions in this file, check if we've already seen this symbol
    if (is_definition && !is.null(seen_symbols_env[[symbol_id]])) {
      is_definition <- FALSE
    }

    role <- if (is_definition) "definition" else "reference"

    if (is_definition) {
      symbols[[length(symbols) + 1]] <- list(
        symbol = symbol_id,
        display_name = name,
        kind = kind,
        documentation = extract_roxygen_documentation(data, row$line1)
      )
      seen_symbols_env[[symbol_id]] <- TRUE

      # Track scope for function definitions to build relationships later
      if (kind == "function") {
        scope_info <- get_function_scope(row, data, tree_index)
        if (!is.null(scope_info)) {
          definition_scopes[[symbol_id]] <- scope_info
        }
      }
    }

    occurrences[[length(occurrences) + 1]] <- list(
      symbol = symbol_id,
      range = c(row$line1, row$col1, row$line2, row$col2),
      role = role
    )
  }

  # Build relationships: for each function definition, find references within its scope
  t_rel <- bench_start("build_relationships", list(symbol_count = length(symbols), scope_count = length(definition_scopes)))
  symbols <- build_symbol_relationships(symbols, occurrences, definition_scopes)
  bench_end(t_rel)

  # Extract S3 symbols (generics and methods)
  t_s3 <- bench_start("extract_s3_symbols")
  s3_symbols <- build_s3_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  bench_end(t_s3, list(s3_count = length(s3_symbols)))
  
  if (length(s3_symbols) > 0) {
    # Merge S3 symbols into main symbol list
    # Check for duplicates - prefer S3-specific kind over generic function
    for (s3_sym in s3_symbols) {
      existing_idx <- which(sapply(symbols, function(s) s$symbol == s3_sym$symbol))
      
      if (length(existing_idx) > 0) {
        # Update existing symbol with S3-specific kind
        symbols[[existing_idx]]$kind <- s3_sym$kind
      } else {
        # Add new S3 symbol
        symbols[[length(symbols) + 1]] <- s3_sym
      }
    }
  }

  # Extract R6 class symbols (classes, methods, fields)
  t_r6 <- bench_start("extract_r6_symbols")
  r6_symbols <- build_r6_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  bench_end(t_r6, list(r6_count = length(r6_symbols)))
  
  if (length(r6_symbols) > 0) {
    for (r6_sym in r6_symbols) {
      symbols[[length(symbols) + 1]] <- r6_sym
    }
  }

  # Extract S4 class symbols (classes, generics, methods, slots)
  t_s4 <- bench_start("extract_s4_symbols")
  s4_symbols <- build_s4_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  bench_end(t_s4, list(s4_count = length(s4_symbols)))
  
  if (length(s4_symbols) > 0) {
    for (s4_sym in s4_symbols) {
      symbols[[length(symbols) + 1]] <- s4_sym
    }
  }

  # Extract Shiny symbols (modules, reactives, observers, outputs)
  t_shiny <- bench_start("extract_shiny_symbols")
  shiny_symbols <- build_shiny_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  bench_end(t_shiny, list(shiny_count = length(shiny_symbols)))
  
  if (length(shiny_symbols) > 0) {
    for (shiny_sym in shiny_symbols) {
      symbols[[length(symbols) + 1]] <- shiny_sym
    }
  }

  # Extract package-qualified external symbols (pkg::symbol and pkg:::symbol)
  external_refs <- list()
  if (track_externals) {
    t_qual <- bench_start("extract_qualified_symbols")
    qualified_symbols <- extract_qualified_symbols(ast_module)
    bench_end(t_qual, list(qualified_count = length(qualified_symbols)))
    
    # Build external references grouped by package
    for (qual in qualified_symbols) {
      pkg_name <- qual$package
      
      # Find or create package entry in external_refs
      if (length(external_refs) == 0) {
        pkg_idx <- integer(0)
      } else {
        pkg_matches <- sapply(external_refs, function(x) identical(x$package, pkg_name))
        pkg_idx <- which(pkg_matches)
      }
      
      if (length(pkg_idx) == 0) {
        # Create new package entry
        # Safe access: use named indexing with default fallback
        dep_version <- if (pkg_name %in% names(workspace$dependencies)) {
          workspace$dependencies[[pkg_name]]
        } else {
          "0.0.0"
        }
        external_refs[[length(external_refs) + 1]] <- list(
          package = pkg_name,
          version = dep_version,
          symbols = list(list(
            name = qual$token,
            accessor = qual$accessor,
            is_internal = (qual$accessor == ":::"),
            range = qual$range
          ))
        )
      } else {
        # Add to existing package entry
        external_refs[[pkg_idx[1]]]$symbols[[length(external_refs[[pkg_idx[1]]]$symbols) + 1]] <- list(
          name = qual$token,
          accessor = qual$accessor,
          is_internal = (qual$accessor == ":::"),
          range = qual$range
        )
      }
    }
  }

  list(
    symbols = symbols,
    occurrences = occurrences,
    external_refs = external_refs
  )
}

#' Build a unique SCIP symbol identifier (legacy fallback).
#'
#' @description
#' This function is kept for backwards compatibility but is deprecated.
#' Use `resolve_symbol_id()` with a symbol table for proper cross-file resolution.
#'
#' @param name Symbol name.
#' @param relative_path Document-relative path.
#' @param namespace Optional namespace prefix.
#' @return Symbol identifier string following SCIP local symbol format.
#' @keywords internal
build_symbol_id <- function(name, relative_path, namespace = NULL) {
  ns_prefix <- if (!is.null(namespace) && nzchar(namespace)) paste0(namespace, "/") else ""
  sanitized_path <- gsub("[^A-Za-z0-9_/]", "_", relative_path)
  sanitized_name <- gsub("[^A-Za-z0-9_]", "_", name)
  local_id <- paste0(ns_prefix, sanitized_path, "#", sanitized_name)
  paste("local", local_id)
}

#' Check if a symbol occurrence is inside a function body.
#'
#' Walks up the AST to check if this symbol is nested within a FUNCTION node.
#' Used to determine if a variable should be local-scoped.
#'
#' @param row Parse data row for the symbol.
#' @param parse_data Full parse data frame.
#' @param tree_index Tree index from build_tree_index().
#' @return TRUE if inside a function, FALSE otherwise.
#' @keywords internal
is_inside_function <- function(row, parse_data, tree_index) {
  current_id <- row$id

  while (!is.na(current_id)) {
    parent_id <- get_parent_id_indexed(current_id, tree_index)
    if (is.na(parent_id)) {
      break
    }

    row_idx <- get_row_indexed(parent_id, tree_index)
    if (is.na(row_idx)) {
      break
    }

    parent_token <- parse_data$token[row_idx]

    if (parent_token == "FUNCTION") {
      return(TRUE)
    }

    current_id <- parent_id
  }

  FALSE
}

#' Resolve a symbol to its appropriate SCIP symbol ID.
#'
#' This is the main resolution function that determines the correct symbol ID
#' based on the symbol table, symbol type, and context. It handles:
#' - Package-level definitions (global symbols)
#' - Cross-file references (resolved via symbol table)
#' - Base R functions (external references)
#' - Local variables (file-scoped symbols)
#'
#' @param name Symbol name.
#' @param kind Symbol kind (function, variable, etc.).
#' @param is_definition Whether this is a definition occurrence.
#' @param is_local_scope Whether this symbol is in a local scope (inside function).
#' @param is_call Whether this is a function call.
#' @param relative_path Current file's relative path.
#' @param symbol_table Optional symbol table from build_symbol_table().
#' @param workspace Workspace context.
#' @return List with symbol_id and resolution info.
#' @keywords internal
resolve_symbol_id <- function(name, kind, is_definition, is_local_scope,
                               is_call, relative_path, symbol_table, workspace) {
  t_resolve <- bench_start("phase3_resolve_symbol_id")

  # If no symbol table, fall back to legacy behavior
  if (is.null(symbol_table)) {
    result <- list(
      symbol_id = build_symbol_id(name, relative_path, workspace$metadata$namespace),
      resolved_to = "local"
    )
    bench_end(t_resolve, list(resolved_to = "local", lookup_type = "no_table"))
    bench_count("phase3_lookup_no_table")
    return(result)
  }

  # For truly local symbols (params, loop vars, vars inside functions),
  # use local symbol IDs unless it's a function call that might resolve globally
  if (is_local_scope && is_definition && kind != "function") {
    result <- list(
      symbol_id = build_local_symbol_id(name, relative_path),
      resolved_to = "local"
    )
    bench_end(t_resolve, list(resolved_to = "local", lookup_type = "local_scope"))
    bench_count("phase3_lookup_local_scope")
    return(result)
  }

  # Check symbol table for package-level definitions
  t_lookup <- bench_start("phase3_symbol_table_lookup", list(name = name))
  found_in_table <- name %in% names(symbol_table$definitions)
  bench_end(t_lookup, list(found = found_in_table))

  if (found_in_table) {
    def <- symbol_table$definitions[[name]]
    result <- list(
      symbol_id = def$symbol_id,
      resolved_to = symbol_table$package_name,
      is_exported = def$exported,
      defined_in = def$file
    )
    bench_end(t_resolve, list(resolved_to = symbol_table$package_name, lookup_type = "table_hit"))
    bench_count("phase3_lookup_table_hit")
    return(result)
  }

  bench_count("phase3_lookup_table_miss")

  # For function calls, check if it's a base R function
  if (is_call && is_base_r_function(name)) {
    result <- list(
      symbol_id = build_external_symbol_id(name, "base", get_r_version(), "function"),
      resolved_to = "base"
    )
    bench_end(t_resolve, list(resolved_to = "base", lookup_type = "base_r"))
    bench_count("phase3_lookup_base_r")
    return(result)
  }

  # Unresolved - use local symbol ID (file-scoped)
  # This handles:
  # - Variables that aren't in the symbol table
  # - Functions from attached packages (library() calls) that we can't resolve
  result <- list(
    symbol_id = build_local_symbol_id(name, relative_path),
    resolved_to = "local"
  )
  bench_end(t_resolve, list(resolved_to = "local", lookup_type = "unresolved"))
  bench_count("phase3_lookup_unresolved")
  result
}

#' Fast symbol ID resolution using environment lookups.
#'
#' OPTIMIZATION: Uses O(1) environment lookups instead of named list access.
#' Implements lazy local symbol creation - only creates local symbols when needed.
#'
#' @param name Symbol name.
#' @param kind Symbol kind.
#' @param is_definition Whether this is a definition.
#' @param is_local_scope Whether in local scope.
#' @param is_call Whether this is a function call.
#' @param relative_path Current file path.
#' @param definitions_env Environment from symbol_table$definitions_env (or NULL).
#' @param local_registry Environment for lazy local symbol creation.
#' @param local_counter_env Environment containing local_counter variable.
#' @return Symbol ID string.
#' @keywords internal
resolve_symbol_id_fast <- function(name, kind, is_definition, is_local_scope,
                                    is_call, relative_path, definitions_env,
                                    local_registry, local_counter_env) {
  bench_count("phase3_resolve_fast")

  # If no definitions_env, fall back to local symbol
  if (is.null(definitions_env)) {
    bench_count("phase3_fast_no_table")
    return(build_local_symbol_id(name, relative_path))
  }

  # For truly local symbols (params, loop vars, vars inside functions),
  # use local symbol IDs unless it's a function call that might resolve globally
  if (is_local_scope && is_definition && kind != "function") {
    bench_count("phase3_fast_local_def")
    return(build_local_symbol_id(name, relative_path))
  }

  # O(1) lookup in definitions environment
  def <- definitions_env[[name]]
  if (!is.null(def)) {
    bench_count("phase3_fast_table_hit")
    return(def$symbol_id)
  }

  # For function calls, check base R
  if (is_call && is_base_r_function(name)) {
    bench_count("phase3_fast_base_r")
    return(build_external_symbol_id(name, "base", get_r_version(), "function"))
  }

  # LAZY LOCAL SYMBOL CREATION:
  # Create local symbol on-demand, deduplicate by name within file
  bench_count("phase3_fast_local_lazy")
  local_key <- paste0(relative_path, "#", name)
  existing <- local_registry[[local_key]]
  if (!is.null(existing)) {
    return(existing)
  }

  # Create new local symbol
  symbol_id <- build_local_symbol_id(name, relative_path)
  local_registry[[local_key]] <- symbol_id
  symbol_id
}

#' Check if symbol is defined as a function.
#'
#' Uses tree index for O(1) lookups and early-exit DFS.
#'
#' @param row Parse-data row for the symbol.
#' @param parse_data Full parse-data frame.
#' @param index Tree index from build_tree_index().
#' @return Logical indicating if this is a function definition.
#' @keywords internal
is_function_definition_indexed <- function(row, parse_data, index) {
  line <- row$line1

  # FAST PATH: If no FUNCTION/OP-LAMBDA token on this line, can't be a function definition
  # This avoids expensive subtree traversal for ~95% of cases
  line_key <- as.character(line)
  if (length(index$has_func_on_line) == 0 || !line_key %in% names(index$has_func_on_line)) {
    return(FALSE)
  }

  # Find assignment on same line after this symbol - use indexed O(1) line lookup
  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  assignment_rows <- find_on_same_line_indexed(
    row$id,
    index,
    assignment_tokens,
    "after"
  )

  if (nrow(assignment_rows) == 0) {
    return(FALSE)
  }

  # Get the closest assignment (smallest column position)
  assignment_rows <- assignment_rows[order(assignment_rows$col1), , drop = FALSE]
  assignment_row <- assignment_rows[1, ]

  # Get the expression containing the assignment - use indexed ancestor lookup
  assignment_expr <- find_ancestor_of_type_indexed(assignment_row$id, "expr", index)

  if (nrow(assignment_expr) == 0) {
    return(FALSE)
  }

  # Check if the expression contains FUNCTION or OP-LAMBDA - use indexed early-exit DFS
  has_descendant_of_type_indexed(
    assignment_expr$id,
    c("FUNCTION", "OP-LAMBDA"),
    index
  )
}

#' Determine the general kind for a symbol token.
#'
#' Uses tree index for O(1) lookups and early-exit DFS.
#'
#' @param row Parse-data row for the symbol.
#' @param parse_data Full parse-data frame.
#' @param index Tree index from build_tree_index().
#' @return Character label describing the symbol kind.
#' @keywords internal
determine_symbol_kind_indexed <- function(row, parse_data, index) {
  # FIX 1: Function parameters (SYMBOL_FORMALS) - HIGH IMPACT
  if (row$token == "SYMBOL_FORMALS") {
    return("parameter")
  }

  # FIX 2: Loop variables (forcond parent) - use indexed lookup
  parent_id <- get_parent_id_indexed(row$id, index)
  if (!is.na(parent_id)) {
    row_idx <- get_row_indexed(parent_id, index)
    if (!is.na(row_idx) && parse_data$token[row_idx] == "forcond") {
      return("variable")
    }
  }

  # FIX 3: Robust function detection - use indexed version
  if (is_function_definition_indexed(row, parse_data, index)) {
    return("function")
  }

  # Function calls
  if (row$token == "SYMBOL_FUNCTION_CALL") {
    return("call")
  }

  # Default: regular variable
  "variable"
}

#' Merge external reference lists from multiple files.
#'
#' Combines external reference lists, merging symbols for the same package.
#'
#' @param refs1 First external references list.
#' @param refs2 Second external references list to merge.
#' @return Merged external references list.
#' @keywords internal
merge_external_refs <- function(refs1, refs2) {
  if (length(refs1) == 0) return(refs2)
  if (length(refs2) == 0) return(refs1)

  result <- refs1

  for (pkg2 in refs2) {
    # Find matching package in refs1
    pkg_idx <- which(sapply(result, function(x) x$package == pkg2$package))

    if (length(pkg_idx) == 0) {
      # New package, add it
      result[[length(result) + 1]] <- pkg2
    } else {
      # Merge symbols for existing package
      result[[pkg_idx]]$symbols <- c(result[[pkg_idx]]$symbols, pkg2$symbols)
    }
  }

  result
}

#' Get the scope (range) of a function definition.
#'
#' Given a symbol row that defines a function, finds the expression containing
#' the entire function definition (including the body) and returns its range.
#'
#' @param row Parse-data row for the symbol being defined.
#' @param parse_data Full parse-data frame.
#' @param index Tree index from build_tree_index().
#' @return List with line1, col1, line2, col2 of the function scope, or NULL if not found.
#' @keywords internal
get_function_scope <- function(row, parse_data, index) {
  # Find the assignment expression containing this symbol
  # The function body will be a descendant of this expression
  assignment_row <- find_on_same_line_indexed(row$id, index, c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"), "after")

  if (nrow(assignment_row) == 0) {
    return(NULL)
  }

  # Get the parent expression that contains the entire assignment
  expr_row <- find_ancestor_of_type_indexed(assignment_row$id[1], "expr", index)

  if (nrow(expr_row) == 0) {
    return(NULL)
  }

  list(
    node_id = expr_row$id[1],
    line1 = expr_row$line1[1],
    col1 = expr_row$col1[1],
    line2 = expr_row$line2[1],
    col2 = expr_row$col2[1]
  )
}

#' Build relationships for symbols based on their scopes.
#'
#' For each symbol with a defined scope (e.g., function definitions), finds all
#' reference occurrences within that scope and adds them as relationships.
#'
#' @param symbols List of symbol entries from resolve_symbols().
#' @param occurrences List of occurrence entries from resolve_symbols().
#' @param definition_scopes Named list mapping symbol_id to scope info.
#' @return Updated symbols list with relationships field populated.
#' @keywords internal
build_symbol_relationships <- function(symbols, occurrences, definition_scopes) {
  if (length(definition_scopes) == 0 || length(occurrences) == 0) {
    return(symbols)
  }

  # For efficiency, build a lookup from symbol_id to symbol index
  symbol_idx_map <- stats::setNames(
    seq_along(symbols),
    vapply(symbols, function(s) s$symbol, character(1))
  )

  # Filter to reference occurrences only
  ref_occurrences <- Filter(function(occ) identical(occ$role, "reference"), occurrences)

  # For each definition with a scope, find references within that scope
  for (def_symbol_id in names(definition_scopes)) {
    scope <- definition_scopes[[def_symbol_id]]
    sym_idx <- symbol_idx_map[[def_symbol_id]]

    if (is.null(sym_idx)) next

    # Find references that fall within this scope
    # A reference is in scope if its range is contained within the scope range
    refs_in_scope <- list()
    seen_ref_symbols <- character()

    for (ref_occ in ref_occurrences) {
      ref_range <- ref_occ$range
      ref_line <- ref_range[1]
      ref_col <- ref_range[2]

      # Check if reference is within scope (simple range containment)
      in_scope <- (
        (ref_line > scope$line1 || (ref_line == scope$line1 && ref_col >= scope$col1)) &&
        (ref_line < scope$line2 || (ref_line == scope$line2 && ref_col <= scope$col2))
      )

      if (in_scope) {
        ref_symbol <- ref_occ$symbol

        # Skip self-references and duplicates
        if (ref_symbol != def_symbol_id && !ref_symbol %in% seen_ref_symbols) {
          refs_in_scope[[length(refs_in_scope) + 1]] <- list(
            symbol = ref_symbol,
            is_reference = TRUE
          )
          seen_ref_symbols <- c(seen_ref_symbols, ref_symbol)
        }
      }
    }

    # Add relationships to the symbol entry
    if (length(refs_in_scope) > 0) {
      symbols[[sym_idx]]$relationships <- refs_in_scope
    }
  }

  symbols
}
