#' @include utils.R
#' @include parser.R
#' @include documentation.R
#' @include parse-tree-utils.R
#' @include symbols-qualified.R
#' @include symbols-s3.R
#' @include symbols-s4.R
#' @include symbols-r6.R
#' @include symbols-shiny.R
NULL

# This script derives symbol definitions and relationships from parsed modules.
# It maps R constructs into SCIP-compatible descriptors and metadata bundles.
# Integrated with symbols-qualified.R for package-qualified symbol tracking.

#' Resolve symbols present in an AST module.
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @param workspace Workspace context with metadata and dependencies.
#' @param track_externals Logical; if TRUE, track package-qualified external symbols (default: TRUE).
#' @return A list containing symbol information, occurrence records, and external references.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' resolve_symbols(ast, analyze_workspace(tempdir()))
#' resolve_symbols(ast, analyze_workspace(tempdir()), track_externals = FALSE)
#' }
resolve_symbols <- function(ast_module, workspace, track_externals = TRUE) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list(symbols = list(), occurrences = list(), external_refs = list()))
  }

  file_info <- ast_module$file_info %||% list(relative_path = basename(ast_module$file_info$path))
  relative_path <- file_info$relative_path %||% basename(file_info$path)

  # Build tree index once for O(1) lookups throughout this function
  tree_index <- build_tree_index(data)

  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  assignment_rows <- data[data$token %in% assignment_tokens, , drop = FALSE]
  symbols <- list()
  occurrences <- list()
  seen_symbols <- character()

  # Track definition scopes for relationship building
  # Maps symbol_id -> list(node_id = expr_id, line1, col1, line2, col2)
  definition_scopes <- list()

  # FIX 1: Include SYMBOL_FORMALS to detect function parameters
  symbol_rows <- data[data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"), ]
  symbol_rows <- symbol_rows[order(symbol_rows$line1, symbol_rows$col1), , drop = FALSE]

  for (idx in seq_len(nrow(symbol_rows))) {
    row <- symbol_rows[idx, ]
    name <- row$text
    symbol_id <- build_symbol_id(name, relative_path, workspace$metadata$namespace)

    # Determine if this is a definition
    # SYMBOL_FORMALS are always definitions (function parameters)
    if (row$token == "SYMBOL_FORMALS") {
      is_definition <- !(symbol_id %in% seen_symbols)
    } else {
      # FIX 2: Check for loop variables (forcond parent) - use indexed lookup
      parent_id <- get_parent_id_indexed(row$id, tree_index)
      parent_token <- if (!is.na(parent_id)) {
        row_idx <- get_row_indexed(parent_id, tree_index)
        if (!is.na(row_idx)) data$token[row_idx] else NA
      } else {
        NA
      }
      is_loop_var <- !is.na(parent_token) && parent_token == "forcond"

      # Check for assignment
      on_assignment_line <- any(assignment_rows$line1 == row$line1 & assignment_rows$col1 > row$col1)

      is_definition <- (is_loop_var || (row$token == "SYMBOL" && on_assignment_line)) &&
                       !(symbol_id %in% seen_symbols)
    }

    role <- if (is_definition) "definition" else "reference"
    kind <- determine_symbol_kind_indexed(row, data, tree_index)

    if (is_definition) {
      symbols[[length(symbols) + 1]] <- list(
        symbol = symbol_id,
        display_name = name,
        kind = kind,
        documentation = extract_roxygen_documentation(data, row$line1)
      )
      seen_symbols <- c(seen_symbols, symbol_id)

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
  symbols <- build_symbol_relationships(symbols, occurrences, definition_scopes)

  # Extract S3 symbols (generics and methods)
  s3_symbols <- build_s3_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
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
  r6_symbols <- build_r6_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
  if (length(r6_symbols) > 0) {
    for (r6_sym in r6_symbols) {
      symbols[[length(symbols) + 1]] <- r6_sym
    }
  }

  # Extract S4 class symbols (classes, generics, methods, slots)
  s4_symbols <- build_s4_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
  if (length(s4_symbols) > 0) {
    for (s4_sym in s4_symbols) {
      symbols[[length(symbols) + 1]] <- s4_sym
    }
  }

  # Extract Shiny symbols (modules, reactives, observers, outputs)
  shiny_symbols <- build_shiny_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
  if (length(shiny_symbols) > 0) {
    for (shiny_sym in shiny_symbols) {
      symbols[[length(symbols) + 1]] <- shiny_sym
    }
  }

  # Extract package-qualified external symbols (pkg::symbol and pkg:::symbol)
  external_refs <- list()
  if (track_externals) {
    qualified_symbols <- extract_qualified_symbols(ast_module)
    
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

#' Build a unique SCIP symbol identifier.
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
