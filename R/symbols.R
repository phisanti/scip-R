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

# =============================================================================
# SYMBOL RESOLUTION MODULE
# =============================================================================
#
# This module extracts symbol definitions and references from parsed R ASTs,
# producing SCIP-compatible symbol information and occurrence records.
#
# ARCHITECTURE:
#   1. Core symbol resolution has two implementations:
#      - C++ (src/tree_ops.cpp): Used by default for performance (~2x faster)
#      - R (this file): Serves as fallback and reference implementation
#
#   2. Specialized symbol extraction (always in R):
#      - S3 generics/methods: symbols-s3.R
#      - S4 classes/methods: symbols-s4.R
#      - R6 classes: symbols-r6.R
#      - Shiny modules: symbols-shiny.R
#      - Package-qualified refs: symbols-qualified.R
#
# MAINTENANCE:
#   - The C++ implementation mirrors the R logic; keep them in sync
#   - Test changes with: R CMD INSTALL . && index playbase
#   - Compare outputs with: scripts/compare_scip_indexes.R
#
# SEE ALSO:
#   - src/tree_ops.cpp (Phase 4B section) for C++ implementation
#   - symbol-table.R for cross-file resolution infrastructure
# =============================================================================

#' Resolve symbols present in an AST module.
#'
#' Analyzes parsed R code to extract symbol definitions and references,
#' producing SCIP-compatible symbol information. Uses a C++ implementation
#' for performance with automatic R fallback.
#'
#' @section Cross-file Resolution:
#' When a symbol_table is provided, references to symbols defined in other
#' files will resolve to the same global symbol ID as the definition. This
#' enables "go to definition" across files.
#'
#' @section Performance:
#' The core resolution loop is implemented in C++ (see `src/tree_ops.cpp`,
#' Phase 4B section) for ~2x overall speedup. The R implementation below
#' serves as fallback and reference.
#'
#' @param ast_module List produced by `parse_file()` containing:
#'   - `parse_data`: DataFrame from getParseData()
#'   - `file_info`: List with path and relative_path
#' @param workspace Workspace context from `analyze_workspace()` with
#'   metadata and dependency information.
#' @param track_externals Logical; if TRUE (default), extract package-qualified
#'   symbols like `pkg::func` and `pkg:::internal`.
#' @param symbol_table Optional symbol table from `build_symbol_table()`.
#'   When provided, enables cross-file reference resolution.
#' @param tree_index Optional pre-built tree index from `build_tree_index()`.
#'   If NULL, one will be built. Passing a cached index improves performance.
#'
#' @return A list with three components:
#'   \describe{
#'     \item{symbols}{List of symbol definitions, each with:
#'       symbol (ID), display_name, kind, documentation, relationships}
#'     \item{occurrences}{List of all symbol uses, each with:
#'       symbol (ID), range (line1, col1, line2, col2), role ("definition"/"reference")}
#'     \item{external_refs}{Package-qualified references grouped by package}
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' workspace <- analyze_workspace(tempdir())
#'
#' # Basic usage (no cross-file resolution)
#' result <- resolve_symbols(ast, workspace)
#'
#' # With cross-file resolution
#' symbol_table <- build_symbol_table(parsed_files, workspace)
#' result <- resolve_symbols(ast, workspace, symbol_table = symbol_table)
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

  # Get definitions_env from symbol_table for O(1) lookups
  definitions_env <- if (!is.null(symbol_table)) symbol_table$definitions_env else NULL

  # ============================================================================
  # C++ IMPLEMENTATION (Phase 4B)
  # ============================================================================
  # The C++ implementation in src/tree_ops.cpp provides ~2x overall speedup
  # by moving the hot loop (symbol iteration + resolution) to native code.
  #
  # Prerequisites for C++ path:
  #   1. Rcpp must be available (rcpp_available())
  #   2. Tree index must have C++ pointer (tree_index$cpp_index)
  #   3. resolve_symbols_cpp must be loaded
  #
  # If C++ fails, we fall back to the R implementation below (same algorithm).
  # ============================================================================
  cpp_result <- NULL
  use_cpp <- rcpp_available() &&
             !is.null(tree_index$cpp_index) &&
             exists("resolve_symbols_cpp", mode = "function")

  if (use_cpp) {
    t_cpp <- bench_start("resolve_symbols_cpp", list(row_count = nrow(data)))
    cpp_result <- tryCatch(
      resolve_symbols_cpp(
        parse_data = data,
        tree_index_ptr = tree_index$cpp_index,
        inside_function = tree_index$inside_function %||% logical(nrow(data)),
        enclosing_function = tree_index$enclosing_function %||% integer(nrow(data)),
        definitions_env = definitions_env %||% new.env(hash = TRUE, parent = emptyenv()),
        base_r_funcs = BASE_R_FUNCTIONS,
        relative_path = relative_path,
        package_name = if (!is.null(symbol_table)) symbol_table$package_name else "unknown",
        r_version = get_r_version()
      ),
      error = function(e) {
        # Log the error for debugging (visible with SCIPR_BENCH=1)
        bench_count("cpp_resolve_error")
        cli::cli_alert_warning(
          "C++ symbol resolution failed, using R fallback: {conditionMessage(e)}"
        )
        NULL
      }
    )
    bench_end(t_cpp)
  }

  # If C++ succeeded, convert to list format and continue with post-processing
  if (!is.null(cpp_result)) {
    bench_count("cpp_resolve_success")

    # Convert DataFrames to list format
    symbols <- df_to_symbol_list(cpp_result$symbols)
    occurrences <- df_to_occurrence_list(cpp_result$occurrences)

    # NOTE: We no longer build IsReference relationships here.
    # Standard SCIP pattern stores references as Occurrences with SymbolRoles=8,
    # not as SymbolInformation.Relationships. Relationships are reserved for
    # IsImplementation (class inheritance) and IsTypeDefinition only.

    # =========================================================================
    # POST-PROCESSING: Extract roxygen documentation for C++ results
    # =========================================================================
    # The C++ implementation doesn't extract roxygen docs (they require parsing
    # comment tokens which is complex in C++). We extract them here in R.
    #
    # Algorithm:
    #   1. Build symbol_id -> definition_line mapping from occurrences
    #   2. For each symbol with a definition, extract roxygen docs from parse_data
    # =========================================================================

    # Build mapping from symbol ID to definition line
    definition_lines <- new.env(hash = TRUE, parent = emptyenv())
    for (occ in occurrences) {
      if (identical(occ$role, "definition")) {
        # range is c(line1, col1, line2, col2)
        # ranges are 0-based in SCIP; docs extractor expects 1-based lines
        definition_lines[[occ$symbol]] <- occ$range[1] + 1L
      }
    }

    # Extract roxygen documentation for each symbol with a definition
    for (i in seq_along(symbols)) {
      sym_id <- symbols[[i]]$symbol
      def_line <- definition_lines[[sym_id]]
      if (!is.null(def_line)) {
        docs <- extract_roxygen_documentation(data, def_line)
        if (!is.null(docs) && length(docs) > 0) {
          symbols[[i]]$documentation <- docs
        }
      }
    }

  } else {
    # ============================================================================
    # R FALLBACK IMPLEMENTATION
    # ============================================================================
    # This is the reference implementation of symbol resolution. It serves as:
    #   1. Fallback when C++ is unavailable or fails
    #   2. Documentation of the algorithm (readable R code)
    #   3. Baseline for validating C++ implementation correctness
    #
    # The algorithm mirrors resolve_symbols_cpp() in src/tree_ops.cpp exactly.
    # If you change logic here, update the C++ version too!
    # ============================================================================
    bench_count("r_fallback_resolve")

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

    # LAZY LOCAL SYMBOL CREATION: Create local symbols on-demand
    # Maps position_key -> symbol_id for deduplication
    local_symbol_registry <- new.env(hash = TRUE, parent = emptyenv())
    local_counter <- 0L

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
      parent_id <- NA_integer_  # initialized here so qualified_pkg detection below is always safe
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

      # Get enclosing function name for scope encoding in local symbol IDs.
      # This enables distinguishing local vars inside functions (2-hash IDs)
      # from top-level file assignments (1-hash IDs), which is critical for
      # dead code detection to avoid false positives on function-local variables.
      enclosing_name <- if (is_local_scope) {
        get_enclosing_function_name(row$id, tree_index)
      } else {
        NULL
      }

      # Detect pkg::func() or pkg:::func() qualified calls.
      # Parse tree structure: expr -> [SYMBOL_PACKAGE "pkg", NS_GET "::", SYMBOL_FUNCTION_CALL "func"]
      # For SYMBOL tokens (non-call qualified refs like pkg::some_var) the same parent layout applies.
      # SYMBOL_FORMALS cannot be qualified, so we skip them.
      qualified_pkg <- NULL
      if (!is.na(parent_id) && row$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL")) {
        sibling_ids <- get_children_indexed(parent_id, tree_index)
        has_ns_get <- FALSE
        pkg_text <- NULL
        for (sib_id in sibling_ids) {
          sib_row_idx <- get_row_indexed(sib_id, tree_index)
          if (!is.na(sib_row_idx)) {
            sib_token <- data$token[sib_row_idx]
            if (sib_token %in% c("NS_GET", "NS_GET_INT")) {
              has_ns_get <- TRUE
            } else if (sib_token == "SYMBOL_PACKAGE") {
              pkg_text <- data$text[sib_row_idx]
            }
          }
        }
        if (has_ns_get && !is.null(pkg_text)) {
          qualified_pkg <- pkg_text
        }
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
        local_counter_env = environment(),  # For updating local_counter
        enclosing_name = enclosing_name,
        qualified_pkg = qualified_pkg
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

      }

      occurrences[[length(occurrences) + 1]] <- list(
        symbol = symbol_id,
        range = to_scip_range(row$line1, row$col1, row$line2, row$col2),
        role = role
      )
    }

    # NOTE: We no longer build IsReference relationships here.
    # Standard SCIP pattern stores references as Occurrences with SymbolRoles=8,
    # not as SymbolInformation.Relationships. Relationships are reserved for
    # IsImplementation (class inheritance) and IsTypeDefinition only.
  }

  # =========================================================================
  # POST-PROCESSING: List closure definitions
  # =========================================================================
  # Detect list(name = function(...) {...}) patterns at top level and emit
  # them as local SCIP definitions. This runs after both C++ and R paths.
  # =========================================================================
  t_closures <- bench_start("extract_list_closures")
  list_closures <- extract_list_closure_definitions(data, tree_index)
  bench_end(t_closures, list(closure_count = length(list_closures)))

  if (length(list_closures) > 0) {
    for (closure in list_closures) {
      # Build local symbol ID scoped under the parent variable name
      closure_symbol_id <- build_local_symbol_id(
        closure$name, relative_path, closure$parent_name
      )

      # Add SymbolInformation entry
      symbols[[length(symbols) + 1]] <- list(
        symbol = closure_symbol_id,
        display_name = closure$name,
        kind = "function",
        documentation = extract_roxygen_documentation(data, closure$name_range[1])
      )

      # Add definition occurrence at the SYMBOL_SUB position
      # Range uses 0-based indexing for SCIP
      occurrences[[length(occurrences) + 1]] <- list(
        symbol = closure_symbol_id,
        range = c(
          closure$name_range[1] - 1, closure$name_range[2] - 1,
          closure$name_range[3] - 1, closure$name_range[4]
        ),
        role = "definition",
        # Store func_range for EnclosingRange computation
        # This allows the enclosing range pass to use the function body extent
        closure_func_range = c(
          closure$func_range[1] - 1, closure$func_range[2] - 1,
          closure$func_range[3] - 1, closure$func_range[4]
        )
      )
    }
  }

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

  # =========================================================================
  # DYNAMIC DISPATCH DETECTION (Tier 1)
  # =========================================================================
  # Extract call edges from dynamic dispatch patterns:
  #   - Shiny: callModule(), moduleServer()
  #   - Base R: do.call(), match.fun()
  #   - C interface: .Call(), .External()
  #
  # These become SCIP Relationship edges attached to target function symbols.
  # This runs after all symbol extraction so the full symbol list is available
  # for relationship attachment.
  # =========================================================================
  t_dispatch <- bench_start("extract_dynamic_dispatch", list(row_count = nrow(data)))
  dispatch_edges <- extract_dynamic_call_edges(
    data = data,
    tree_index = tree_index,
    definitions_env = definitions_env,
    relative_path = relative_path
  )
  bench_end(t_dispatch)

  # Attach dispatch edges to target symbols as relationships
  if (length(dispatch_edges) > 0) {
    # Build map: symbol_id -> list of edges targeting that symbol
    edge_map <- new.env(hash = TRUE, parent = emptyenv())

    for (edge in dispatch_edges) {
      target_symbol <- edge$symbol
      if (!is.null(target_symbol) && nzchar(target_symbol)) {
        if (is.null(edge_map[[target_symbol]])) {
          edge_map[[target_symbol]] <- list()
        }
        edge_map[[target_symbol]][[length(edge_map[[target_symbol]]) + 1]] <- edge
      }
    }

    # Attach edges to corresponding symbol entries
    for (i in seq_along(symbols)) {
      sym_id <- symbols[[i]]$symbol
      edges_for_symbol <- edge_map[[sym_id]]

      if (!is.null(edges_for_symbol)) {
        # Convert edges to relationship structures
        relationships <- lapply(edges_for_symbol, function(edge) {
          list(
            symbol = edge$symbol,
            is_reference = TRUE,
            confidence = edge$confidence,
            dispatch_type = edge$dispatch_type
          )
        })

        # Attach to symbol entry
        if (is.null(symbols[[i]]$relationships)) {
          symbols[[i]]$relationships <- relationships
        } else {
          symbols[[i]]$relationships <- c(symbols[[i]]$relationships, relationships)
        }
      }
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
            kind = if (identical(qual$token_type, "SYMBOL_FUNCTION_CALL")) "function" else "variable",
            range = qual$range
          ))
        )
      } else {
        # Add to existing package entry
        external_refs[[pkg_idx[1]]]$symbols[[length(external_refs[[pkg_idx[1]]]$symbols) + 1]] <- list(
          name = qual$token,
          accessor = qual$accessor,
          is_internal = (qual$accessor == ":::"),
          kind = if (identical(qual$token_type, "SYMBOL_FUNCTION_CALL")) "function" else "variable",
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
                                    local_registry, local_counter_env,
                                    enclosing_name = NULL, qualified_pkg = NULL) {
  bench_count("phase3_resolve_fast")

  # Explicit pkg::func() — highest priority; the programmer stated the package.
  # Check definitions_env first — if the qualified package matches our own package,
  # use the definition's symbol_id (which has the correct version).
  if (!is.null(qualified_pkg)) {
    bench_count("phase3_fast_qualified_pkg")
    if (!is.null(definitions_env)) {
      def <- definitions_env[[name]]
      if (!is.null(def) && grepl(qualified_pkg, def$symbol_id, fixed = TRUE)) {
        return(def$symbol_id)
      }
    }
    return(build_external_symbol_id(name, qualified_pkg, "0.0.0", kind))
  }

  # If no definitions_env, fall back to local symbol
  if (is.null(definitions_env)) {
    bench_count("phase3_fast_no_table")
    return(build_local_symbol_id(name, relative_path, scope_id = enclosing_name))
  }

  # For truly local symbols (params, loop vars, vars inside functions),
  # use local symbol IDs unless it's a function call that might resolve globally
  if (is_local_scope && is_definition && kind != "function") {
    bench_count("phase3_fast_local_def")
    return(build_local_symbol_id(name, relative_path, scope_id = enclosing_name))
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
  # Create local symbol on-demand, deduplicate by name+scope within file
  bench_count("phase3_fast_local_lazy")
  # Include scope in dedup key to avoid collisions between same-named vars
  # in different functions (e.g., x inside func_a vs x inside func_b)
  if (!is.null(enclosing_name) && nzchar(enclosing_name)) {
    local_key <- paste0(relative_path, "#", enclosing_name, "#", name)
  } else {
    local_key <- paste0(relative_path, "#", name)
  }
  existing <- local_registry[[local_key]]
  if (!is.null(existing)) {
    return(existing)
  }

  # Create new local symbol
  symbol_id <- build_local_symbol_id(name, relative_path, scope_id = enclosing_name)
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
  if (!exists(line_key, envir = index$has_func_on_line, inherits = FALSE)) {
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

# NOTE: get_function_scope() and build_symbol_relationships() were removed.
# These functions created IsReference relationships which is non-standard.
# Standard SCIP pattern stores references as Occurrences with SymbolRoles=8,
# not as SymbolInformation.Relationships. Relationships are reserved for
# IsImplementation (class inheritance) and IsTypeDefinition only.
# See: .active_plans/align_scip-R_to_standard.md

# ============================================================================
# SHINY DYNAMIC DISPATCH CALL EDGES
# ============================================================================
# Detects callModule() and moduleServer() patterns and emits SCIP relationship
# edges to their target functions. This enables call graph tools to show that
# callModule(BiomarkerBoard, "id") dispatches to BiomarkerBoard().
#
# Patterns detected:
#   callModule(target, id, ...)   - first arg is the module server function
#   moduleServer(id, target)      - second arg is the module server function
#
# Returns relationship structures that can be attached to a symbol's
# $relationships field for serialization by build_symbol_information_message().
# ============================================================================

#' Extract Shiny dynamic dispatch call edges.
#'
#' Detects callModule() and moduleServer() patterns and creates relationship
#' edges to their target functions. These edges enable call graph navigation
#' from dispatch sites to their target module server functions.
#'
#' @param data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index() (optional).
#' @param definitions_env Symbol table environment for cross-file resolution (optional).
#' @param relative_path Current file path.
#' @return List of relationship structures, each with:
#'   \describe{
#'     \item{target_name}{Character name of the dispatch target function}
#'     \item{symbol}{SCIP symbol ID of the resolved target (or NULL if unresolved)}
#'     \item{is_reference}{TRUE, indicating a "calls" relationship}
#'     \item{confidence}{1.0 for string literal targets, 0.9 for symbol targets}
#'     \item{dispatch_type}{Character: "callModule" or "moduleServer"}
#'     \item{range}{4-element integer vector with source location of the dispatch call}
#'   }
#' @keywords internal
extract_shiny_dispatch_edges <- function(data, tree_index = NULL,
                                         definitions_env = NULL,
                                         relative_path) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # Find all function calls in parse data
  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
  if (nrow(func_calls) == 0) {
    return(list())
  }

  dispatch_functions <- c("callModule", "moduleServer")
  edges <- list()

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    call_text <- call_row$text

    if (!(call_text %in% dispatch_functions)) {
      next
    }

    # Navigate to full call expression (parent traversal)
    # SYMBOL_FUNCTION_CALL -> parent expr -> parent's parent (full call)
    call_expr_id <- call_row$parent
    full_call_id <- data[data$id == call_expr_id, ]$parent[1]

    if (is.na(full_call_id)) {
      next
    }

    # Get all descendants of the full call expression
    descendants <- get_descendants(full_call_id, data)

    # Extract target function name based on argument position:
    #   callModule(target, id, ...)  -> target is first arg
    #   moduleServer(id, target)     -> target is second arg
    target_info <- extract_shiny_dispatch_target_arg(
      call_text, full_call_id, descendants, data
    )

    if (is.null(target_info)) {
      next
    }

    # Resolve target name to SCIP symbol ID
    resolved_id <- resolve_shiny_dispatch_target(
      target_info$name, definitions_env, relative_path
    )

    if (is.null(resolved_id)) {
      next
    }

    # Build relationship structure
    edges[[length(edges) + 1]] <- list(
      target_name = target_info$name,
      symbol = resolved_id,
      is_reference = TRUE,
      confidence = target_info$confidence,
      dispatch_type = call_text,
      range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
    )
  }

  edges
}

#' Extract the dispatch target argument from a Shiny dispatch call.
#'
#' Navigates the parse tree to find the correct argument based on position:
#' callModule(target, id, ...) uses the first argument, while
#' moduleServer(id, target) uses the second argument.
#'
#' @param call_text The function name ("callModule" or "moduleServer").
#' @param full_call_id ID of the full call expression in parse data.
#' @param descendants All descendant rows of the call expression.
#' @param data Full parse data data.frame.
#' @return List with name (character) and confidence (numeric), or NULL.
#' @keywords internal
extract_shiny_dispatch_target_arg <- function(call_text, full_call_id, descendants, data) {
  # Get direct children of the full call expression to find argument exprs
  children <- data[data$parent == full_call_id, , drop = FALSE]

  # Argument expressions are 'expr' children, excluding the function name expr.
  # The first expr child is the function call itself (contains SYMBOL_FUNCTION_CALL),
  # subsequent expr children are the arguments.
  expr_children <- children[children$token == "expr", , drop = FALSE]
  expr_children <- expr_children[order(expr_children$col1), , drop = FALSE]

  # Remove the function name expr (first one, which contains the call name)
  if (nrow(expr_children) < 2) {
    return(NULL)
  }
  arg_exprs <- expr_children[-1, , drop = FALSE]

  # Select the target argument based on dispatch function:
  #   callModule(target, id, ...) -> arg 1 (index 1)
  #   moduleServer(id, target)    -> arg 2 (index 2)
  target_arg_idx <- if (call_text == "callModule") 1L else 2L

  if (nrow(arg_exprs) < target_arg_idx) {
    return(NULL)
  }

  target_expr <- arg_exprs[target_arg_idx, ]

  # Get descendants of the target argument expr
  target_descendants <- get_descendants(target_expr$id, data)
  # Include the expr itself in case it has direct token children
  target_all <- rbind(target_descendants, target_expr)

  # Try STR_CONST first (string literal target: confidence 1.0)
  str_const_rows <- target_all[target_all$token == "STR_CONST", , drop = FALSE]
  if (nrow(str_const_rows) > 0) {
    target_name <- gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
    return(list(name = target_name, confidence = 1.0))
  }

  # Fallback to SYMBOL (variable reference: confidence 0.9)
  symbol_rows <- target_all[target_all$token == "SYMBOL", , drop = FALSE]
  if (nrow(symbol_rows) > 0) {
    return(list(name = symbol_rows$text[1], confidence = 0.9))
  }

  NULL
}

#' Resolve Shiny dispatch target to SCIP symbol ID.
#'
#' Looks up target function name in symbol table and constructs SCIP symbol ID.
#' Tries cross-file resolution first via definitions_env, then falls back to
#' constructing a local symbol ID.
#'
#' @param target_name Function name (string).
#' @param definitions_env Symbol table environment from build_symbol_table() (optional).
#' @param relative_path Current file path.
#' @return SCIP symbol ID string or NULL.
#' @keywords internal
resolve_shiny_dispatch_target <- function(target_name, definitions_env, relative_path) {
  if (is.null(target_name) || !nzchar(target_name)) {
    return(NULL)
  }

  # Try symbol table first (cross-file resolution)
  if (!is.null(definitions_env)) {
    def <- definitions_env[[target_name]]
    if (!is.null(def)) {
      return(def$symbol_id)
    }
  }

  # Fallback: Construct local symbol ID for same-file or unresolved targets
  build_local_symbol_id(target_name, relative_path)
}

# ============================================================================
# LIST CLOSURE DEFINITIONS
# ============================================================================
# Detects anonymous closures assigned as named list elements at top level:
#   MODULE <- list(server = function(id) { ... }, ui = function() { ... })
#
# These closures are emitted as local SCIP definitions with EnclosingRange
# spanning their function body. This enables call graph tools to create
# caller edges for references inside the closure body.
#
# Out of scope: truly anonymous closures (lapply(x, function(x) ...)),
# env assignments (env$foo <- function()), nested lists.
# ============================================================================

#' Extract named function closures from top-level list assignments.
#'
#' Scans parse data for `list(name = function(...) {...})` patterns where the
#' list is assigned at top level (not inside a function body). Returns metadata
#' for each closure suitable for emitting as a SCIP definition occurrence.
#'
#' Skips closures that:
#' - Are inside function bodies (local scope)
#' - Are inside R6Class() or setClass() calls (handled by dedicated modules)
#' - Have non-function RHS values (e.g., `config = list(a = 1)`)
#'
#' @param data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index().
#' @return List of closure definition records, each with:
#'   \describe{
#'     \item{name}{Element name (e.g., "server")}
#'     \item{parent_name}{Enclosing variable name (e.g., "MODULE")}
#'     \item{func_range}{4-element range of the function expr (for EnclosingRange)}
#'     \item{name_range}{4-element range of the SYMBOL_SUB token (for definition occurrence)}
#'   }
#' @keywords internal
extract_list_closure_definitions <- function(data, tree_index) {
  # Find all EQ_SUB tokens (named argument assignments in function calls)
  eq_sub_rows <- data[data$token == "EQ_SUB", , drop = FALSE]
  if (nrow(eq_sub_rows) == 0) {
    return(list())
  }

  inside_func_mask <- tree_index$inside_function %||% logical(nrow(data))
  closures <- list()

  for (i in seq_len(nrow(eq_sub_rows))) {
    eq_row <- eq_sub_rows[i, ]

    # Skip if inside a function body
    row_idx <- get_row_indexed(eq_row$id, tree_index)
    if (!is.na(row_idx) && isTRUE(inside_func_mask[row_idx])) {
      next
    }

    # Get the parent node (should be an expr containing the list() call)
    parent_id <- eq_row$parent
    if (is.na(parent_id)) next

    siblings <- data[data$parent == parent_id, , drop = FALSE]

    # Check that this parent expr is a list() call (not R6Class, setClass, etc.)
    # Look for SYMBOL_FUNCTION_CALL among siblings or their children
    call_name <- find_call_name_in_expr(parent_id, data, tree_index)
    if (is.null(call_name)) next

    # Skip R6Class and S4 patterns (already handled by dedicated modules)
    if (call_name %in% c("R6Class", "setClass", "setRefClass", "setGeneric",
                          "setMethod", "setValidity", "representation")) {
      next
    }

    # Only handle list() calls
    if (call_name != "list") next

    # Find the SYMBOL_SUB before this EQ_SUB (the element name)
    sym_sub_rows <- siblings[
      siblings$token == "SYMBOL_SUB" &
      (siblings$line1 < eq_row$line1 |
       (siblings$line1 == eq_row$line1 & siblings$col1 < eq_row$col1)),
      , drop = FALSE
    ]

    if (nrow(sym_sub_rows) == 0) next

    # Get the closest SYMBOL_SUB (last one before EQ_SUB)
    sym_sub_rows <- sym_sub_rows[order(sym_sub_rows$line1, sym_sub_rows$col1), , drop = FALSE]
    sym_sub <- sym_sub_rows[nrow(sym_sub_rows), ]

    # Find the expr after this EQ_SUB (the RHS value)
    rhs_expr_rows <- siblings[
      siblings$token == "expr" &
      (siblings$line1 > eq_row$line1 |
       (siblings$line1 == eq_row$line1 & siblings$col1 > eq_row$col1)),
      , drop = FALSE
    ]

    if (nrow(rhs_expr_rows) == 0) next

    # Get the closest expr after EQ_SUB
    rhs_expr_rows <- rhs_expr_rows[order(rhs_expr_rows$line1, rhs_expr_rows$col1), , drop = FALSE]
    rhs_expr <- rhs_expr_rows[1, ]

    # Check if the RHS expr contains a FUNCTION or lambda token
    # R 4.1+ uses '\\' token for \(x) lambda syntax (not OP-LAMBDA)
    if (!has_descendant_of_type_indexed(rhs_expr$id, c("FUNCTION", "OP-LAMBDA", "'\\\\'"), tree_index)) {
      next
    }

    # Find the parent variable name (e.g., MODULE in MODULE <- list(...))
    parent_name <- find_list_assignment_name(parent_id, data, tree_index)

    closures[[length(closures) + 1]] <- list(
      name = sym_sub$text,
      parent_name = parent_name,
      func_range = c(rhs_expr$line1, rhs_expr$col1, rhs_expr$line2, rhs_expr$col2),
      name_range = c(sym_sub$line1, sym_sub$col1, sym_sub$line2, sym_sub$col2)
    )
  }

  closures
}

#' Find the function call name for a parent expression.
#'
#' Given an expr node that contains a function call (like `list(...)`),
#' finds the SYMBOL_FUNCTION_CALL token name.
#'
#' @param expr_id ID of the expr node.
#' @param data Parse data data.frame.
#' @param tree_index Tree index for lookups.
#' @return Character name of the function being called, or NULL.
#' @keywords internal
find_call_name_in_expr <- function(expr_id, data, tree_index) {
  children_ids <- get_children_indexed(expr_id, tree_index)
  if (length(children_ids) == 0) return(NULL)

  for (child_id in children_ids) {
    row_idx <- get_row_indexed(child_id, tree_index)
    if (is.na(row_idx)) next

    token <- data$token[row_idx]
    if (token == "SYMBOL_FUNCTION_CALL") {
      return(data$text[row_idx])
    }

    # Check for namespace-qualified calls like R6::R6Class
    # These have an expr child containing SYMBOL_FUNCTION_CALL
    if (token == "expr") {
      grandchildren_ids <- get_children_indexed(child_id, tree_index)
      for (gc_id in grandchildren_ids) {
        gc_row_idx <- get_row_indexed(gc_id, tree_index)
        if (!is.na(gc_row_idx) && data$token[gc_row_idx] == "SYMBOL_FUNCTION_CALL") {
          return(data$text[gc_row_idx])
        }
      }
    }
  }

  NULL
}

#' Find the variable name for a list assignment.
#'
#' Given the expr ID of a list() call, walks up the tree to find the
#' variable being assigned to (e.g., "MODULE" in `MODULE <- list(...)`).
#'
#' @param list_expr_id ID of the list call expr node.
#' @param data Parse data data.frame.
#' @param tree_index Tree index for lookups.
#' @return Character variable name, or NULL if not found.
#' @keywords internal
find_list_assignment_name <- function(list_expr_id, data, tree_index) {
  current_id <- list_expr_id
  max_depth <- 5

  for (d in seq_len(max_depth)) {
    parent_id <- get_parent_id_indexed(current_id, tree_index)
    if (is.na(parent_id)) return(NULL)

    parent_row_idx <- get_row_indexed(parent_id, tree_index)
    if (is.na(parent_row_idx)) return(NULL)

    parent_token <- data$token[parent_row_idx]
    if (parent_token != "expr") {
      current_id <- parent_id
      next
    }

    # Check if this parent expr contains an assignment operator
    children_ids <- get_children_indexed(parent_id, tree_index)
    for (child_id in children_ids) {
      child_row_idx <- get_row_indexed(child_id, tree_index)
      if (is.na(child_row_idx)) next

      child_token <- data$token[child_row_idx]
      if (child_token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
        # Found assignment — find the SYMBOL before the assignment operator
        assign_col <- data$col1[child_row_idx]
        assign_line <- data$line1[child_row_idx]

        for (sib_id in children_ids) {
          sib_row_idx <- get_row_indexed(sib_id, tree_index)
          if (is.na(sib_row_idx)) next

          # Look for expr children that contain a SYMBOL
          if (data$token[sib_row_idx] == "expr" &&
              (data$line1[sib_row_idx] < assign_line ||
               (data$line1[sib_row_idx] == assign_line && data$col1[sib_row_idx] < assign_col))) {
            # Check for SYMBOL inside this expr
            expr_children_ids <- get_children_indexed(sib_id, tree_index)
            for (ec_id in expr_children_ids) {
              ec_row_idx <- get_row_indexed(ec_id, tree_index)
              if (!is.na(ec_row_idx) && data$token[ec_row_idx] == "SYMBOL") {
                return(data$text[ec_row_idx])
              }
            }
          }
        }
      }
    }

    current_id <- parent_id
  }

  NULL
}

# ============================================================================
# BASE R DYNAMIC DISPATCH DETECTION
# ============================================================================
# Detects do.call() and match.fun() patterns and creates relationship edges
# to their target functions. This enables call graph edges for common
# metaprogramming patterns like do.call("mean", list(x)) or match.fun("median").
#
# These edges are emitted as SCIP reference occurrences so downstream tools
# (dead code detection, impact analysis, "find usages") see the dynamic call.
# ============================================================================

#' Extract Base R dynamic dispatch call edges.
#'
#' Detects do.call() and match.fun() patterns and creates relationship
#' edges to their target functions.
#'
#' @param data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index() (optional).
#' @param definitions_env Symbol table environment for cross-file resolution.
#' @param relative_path Current file path.
#' @return List of relationship structures with symbol, is_reference, confidence.
#' @keywords internal
extract_base_dispatch_edges <- function(data, tree_index = NULL,
                                        definitions_env = NULL,
                                        relative_path) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  edges <- list()

  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
  if (nrow(func_calls) == 0) {
    return(list())
  }

  dispatch_functions <- c("do.call", "match.fun")

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    call_text <- call_row$text

    if (!(call_text %in% dispatch_functions)) {
      next
    }

    # Navigate to full call expression
    call_expr_id <- call_row$parent
    if (is.na(call_expr_id)) next

    full_call_id <- data[data$id == call_expr_id, ]$parent[1]
    if (is.na(full_call_id)) {
      full_call_id <- call_expr_id
    }

    descendants <- get_descendants(full_call_id, data)
    if (nrow(descendants) == 0) next

    # Extract the first argument (target function name)
    # For do.call: first arg can be string "mean" or symbol mean
    # For match.fun: first arg usually string "median"
    target_info <- extract_dispatch_target_arg(descendants, call_text)
    if (is.null(target_info)) next

    target_name <- target_info$name
    confidence <- target_info$confidence

    # Resolve target to SCIP symbol ID
    symbol_id <- resolve_base_dispatch_target(
      target_name, definitions_env, relative_path
    )

    if (!is.null(symbol_id)) {
      edges[[length(edges) + 1]] <- list(
        symbol = symbol_id,
        is_reference = TRUE,
        confidence = confidence,
        target_name = target_name,
        dispatch_type = call_text,
        range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
      )
    }
  }

  edges
}

#' Extract the target function argument from a dispatch call.
#'
#' Finds the first string constant or symbol argument in the descendants
#' of a do.call() or match.fun() call, filtering out the dispatch function
#' name itself and common non-target symbols.
#'
#' @param descendants Parse data subset (descendants of the call expression).
#' @param call_text The dispatch function name ("do.call" or "match.fun").
#' @return List with name (character) and confidence (numeric), or NULL.
#' @keywords internal
extract_dispatch_target_arg <- function(descendants, call_text) {
  # Strategy: Look for the first STR_CONST or SYMBOL that appears as an argument
  # String constants get confidence 1.0, symbols get 0.9

  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  if (nrow(str_const_rows) > 0) {
    # Take the first string constant — it's the target function name
    target_name <- gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
    return(list(name = target_name, confidence = 1.0))
  }

  # No string constant — look for a symbol argument
  symbol_rows <- descendants[descendants$token == "SYMBOL", , drop = FALSE]
  # Filter out the dispatch function name itself and common non-target tokens
  exclude_names <- c("do.call", "match.fun", "args", "list",
                      "what", "FUN", "ARGS")
  symbol_rows <- symbol_rows[
    !symbol_rows$text %in% exclude_names, , drop = FALSE
  ]

  if (nrow(symbol_rows) > 0) {
    return(list(name = symbol_rows$text[1], confidence = 0.9))
  }

  # Also check for SYMBOL_FUNCTION_CALL that is NOT the dispatch call itself
  # This handles nested cases like do.call(match.fun("func"), ...)
  inner_calls <- descendants[
    descendants$token == "SYMBOL_FUNCTION_CALL" &
    descendants$text != call_text, , drop = FALSE
  ]
  if (nrow(inner_calls) > 0 && inner_calls$text[1] == "match.fun") {
    # Nested dispatch: do.call(match.fun("func"), ...)
    # The string constant we already checked would be the target
    # If we reach here, there was no string constant, so we can't resolve
    return(NULL)
  }

  NULL
}

#' Resolve Base R dispatch target to SCIP symbol ID.
#'
#' Looks up target function name in symbol table and constructs SCIP symbol ID.
#' Handles both local and base R functions.
#'
#' @param target_name Function name (string).
#' @param definitions_env Symbol table environment (optional).
#' @param relative_path Current file path.
#' @return SCIP symbol ID string or NULL.
#' @keywords internal
resolve_base_dispatch_target <- function(target_name, definitions_env, relative_path) {
  if (is.null(target_name) || !nzchar(target_name)) {
    return(NULL)
  }

  # 1. Try symbol table first (cross-file resolution)
  if (!is.null(definitions_env)) {
    def <- definitions_env[[target_name]]
    if (!is.null(def)) {
      return(def$symbol_id)
    }
  }

  # 2. Check if it's a base R function
  if (is_base_r_function(target_name)) {
    return(build_external_symbol_id(target_name, "base", get_r_version(), "function"))
  }

  # 3. Fallback to local symbol
  build_local_symbol_id(target_name, relative_path)
}

# ============================================================================
# C INTERFACE DYNAMIC DISPATCH DETECTION
# ============================================================================
# Detects .Call() and .External() patterns and creates relationship edges
# to their target C/C++ functions. This enables call graph edges for packages
# that use native code via Rcpp, .Call, or .External interfaces.
#
# C function targets are ALWAYS external references (not in R symbol table).
# They appear as either string literals ("_pkg_func") or backtick-quoted
# symbols (`_pkg_func`) in the first argument position.
#
# These edges are emitted as SCIP reference occurrences so downstream tools
# (dead code detection, impact analysis, "find usages") see the native call.
# ============================================================================

#' Extract C interface dynamic dispatch call edges.
#'
#' Detects .Call() and .External() patterns and creates relationship
#' edges to their target C/C++ functions.
#'
#' Handles two forms of the first argument:
#' \itemize{
#'   \item String literals: `.Call("_pkg_func", ...)`
#'   \item Backtick-quoted symbols: `.Call(\`_pkg_func\`, ...)`
#' }
#'
#' @param data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index() (optional).
#' @param definitions_env Symbol table environment (optional, unused for C).
#' @param relative_path Current file path.
#' @return List of relationship structures with symbol, is_reference, confidence.
#' @keywords internal
extract_c_dispatch_edges <- function(data, tree_index = NULL,
                                     definitions_env = NULL,
                                     relative_path) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  edges <- list()

  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
  if (nrow(func_calls) == 0) {
    return(list())
  }

  c_dispatch_functions <- c(".Call", ".External")

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    call_text <- call_row$text

    if (!(call_text %in% c_dispatch_functions)) {
      next
    }

    # Navigate to full call expression
    # .Call is SYMBOL_FUNCTION_CALL -> parent is expr -> parent is full call expr
    call_expr_id <- call_row$parent
    if (is.na(call_expr_id)) next

    full_call_id <- data[data$id == call_expr_id, ]$parent[1]
    if (is.na(full_call_id)) {
      full_call_id <- call_expr_id
    }

    descendants <- get_descendants(full_call_id, data)
    if (nrow(descendants) == 0) next

    # Extract the first argument (C function name)
    # .Call/.External first arg is ALWAYS a string literal or backtick-quoted symbol
    c_func_name <- extract_c_function_name(descendants, call_text)
    if (is.null(c_func_name)) next

    # Resolve to external C symbol ID
    symbol_id <- resolve_c_dispatch_target(c_func_name, relative_path)

    edges[[length(edges) + 1]] <- list(
      symbol = symbol_id,
      is_reference = TRUE,
      confidence = 1.0,  # String literal / backtick symbol -> always confident
      target_name = c_func_name,
      dispatch_type = call_text,
      range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
    )
  }

  edges
}

#' Extract C function name from the first argument of .Call/.External.
#'
#' Looks for the first STR_CONST (string literal) or backtick-quoted SYMBOL
#' among the descendants of the call expression, excluding the dispatch
#' function name itself.
#'
#' @param descendants Parse data subset (descendants of the call expression).
#' @param call_text The dispatch function name (".Call" or ".External").
#' @return Character C function name, or NULL if not found.
#' @keywords internal
extract_c_function_name <- function(descendants, call_text) {
  # Strategy 1: Look for string constant (most common in handwritten .Call code)
  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  if (nrow(str_const_rows) > 0) {
    return(gsub("^['\"]|['\"]$", "", str_const_rows$text[1]))
  }

  # Strategy 2: Look for backtick-quoted SYMBOL (Rcpp-generated code)
  # In Rcpp-generated RcppExports.R, .Call uses backtick-quoted names like:
  #   .Call(`_scipR_build_tree_index_cpp`, id, parent)
  # R parses backtick-quoted names as SYMBOL tokens with backticks in the text
  symbol_rows <- descendants[descendants$token == "SYMBOL", , drop = FALSE]
  # Filter out common non-target symbols (the .Call/.External itself is
  # SYMBOL_FUNCTION_CALL, not SYMBOL, so no need to filter it)
  if (nrow(symbol_rows) > 0) {
    first_sym <- symbol_rows$text[1]
    # Strip backticks if present (R parse data includes them for quoted names)
    first_sym <- gsub("^`|`$", "", first_sym)
    # C function names typically start with underscore or contain package prefix
    # But we accept any symbol in the first argument position
    return(first_sym)
  }

  # No valid first argument found (dynamic/variable reference)
  NULL
}

#' Resolve C interface dispatch target to SCIP external symbol ID.
#'
#' Constructs an external symbol ID for C/C++ functions. C functions are
#' always external references (not in R symbol table) because they live
#' in compiled native code.
#'
#' @param c_func_name C function name (string, e.g., "_scipR_func").
#' @param relative_path Current file path (for context).
#' @return SCIP external symbol ID string.
#' @keywords internal
resolve_c_dispatch_target <- function(c_func_name, relative_path) {
  if (is.null(c_func_name) || !nzchar(c_func_name)) {
    return(NULL)
  }

  # C functions are external references — they live in compiled native code.
  # Format: "external c <func_name>"
  # This is distinct from R symbol IDs (which use "scip-r r ..." or "local ...")
  esc_name <- escape_scip_identifier(c_func_name)
  sprintf("external c %s", esc_name)
}

# ============================================================================
# UNIFIED DYNAMIC DISPATCH INTERFACE
# ============================================================================
# Combines all specialized dispatch extractors (Shiny, Base R, C interface)
# into a single entry point. Called once per file from resolve_symbols().
# ============================================================================

#' Extract all dynamic dispatch call edges.
#'
#' Unified interface that calls all specialized dispatch extractors:
#' - Shiny: callModule, moduleServer
#' - Base R: do.call, match.fun
#' - C interface: .Call, .External
#'
#' Combines results from all three extractors into a single list of
#' dispatch edges ready for relationship attachment.
#'
#' @param data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index() (optional).
#' @param definitions_env Symbol table environment for cross-file resolution.
#' @param relative_path Current file path.
#' @return Combined list of all dispatch edges, each with:
#'   - symbol: Resolved target symbol ID
#'   - is_reference: TRUE (indicates "calls" relationship)
#'   - confidence: 0.9-1.0 (string literal vs symbol)
#'   - target_name: Original target name (for debugging)
#'   - dispatch_type: "callModule", "do.call", ".Call", etc.
#'   - range: Source location \[line1, col1, line2, col2\]
#' @keywords internal
extract_dynamic_call_edges <- function(data, tree_index = NULL,
                                       definitions_env = NULL,
                                       relative_path) {
  # Call all specialized extractors
  shiny_edges <- extract_shiny_dispatch_edges(
    data, tree_index, definitions_env, relative_path
  )

  base_edges <- extract_base_dispatch_edges(
    data, tree_index, definitions_env, relative_path
  )

  c_edges <- extract_c_dispatch_edges(
    data, tree_index, definitions_env, relative_path
  )

  # Combine all edges
  all_edges <- c(shiny_edges, base_edges, c_edges)

  all_edges
}

# ============================================================================
# C++ RESULT CONVERTERS
# ============================================================================
# These functions convert the DataFrame output from resolve_symbols_cpp()
# to the list format expected by downstream code (serializer, relationships).
#
# The C++ implementation returns DataFrames for efficiency (columnar storage),
# but the rest of the R code expects lists of records. These converters bridge
# that gap.
#
# NOTE: This conversion has some overhead, but it's small compared to the
# time saved by running the main loop in C++.
# ============================================================================

#' Convert C++ symbols DataFrame to list format.
#'
#' Transforms the columnar DataFrame from resolve_symbols_cpp() into a list
#' of symbol records, each containing symbol ID, display name, and kind.
#'
#' @param df DataFrame with columns:
#'   \describe{
#'     \item{symbol}{Character: unique symbol ID}
#'     \item{display_name}{Character: human-readable name}
#'     \item{kind}{Character: "function", "variable", "parameter", "call"}
#'   }
#' @return List of symbol entries, each a list with components:
#'   symbol, display_name, kind, documentation (empty placeholder)
#' @keywords internal
#' @seealso \code{\link{resolve_symbols_cpp}} for the C++ implementation
df_to_symbol_list <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }


  # Note: documentation starts as empty placeholder because C++ doesn't extract

  # roxygen docs. Post-processing in resolve_symbols() populates this field.
  lapply(seq_len(nrow(df)), function(i) {
    list(
      symbol = df$symbol[i],
      display_name = df$display_name[i],
      kind = df$kind[i],
      documentation = list()
    )
  })
}

#' Convert C++ occurrences DataFrame to list format.
#'
#' Transforms the columnar DataFrame from resolve_symbols_cpp() into a list
#' of occurrence records, each containing symbol ID, source range, and role.
#'
#' @param df DataFrame with columns:
#'   \describe{
#'     \item{symbol}{Character: symbol ID this occurrence refers to}
#'     \item{line1, col1}{Integer: start position (1-based)}
#'     \item{line2, col2}{Integer: end position (1-based)}
#'     \item{role}{Character: "definition" or "reference"}
#'   }
#' @return List of occurrence entries, each a list with components:
#'   symbol, range (4-element vector), role
#' @keywords internal
#' @seealso \code{\link{resolve_symbols_cpp}} for the C++ implementation
df_to_occurrence_list <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(df)), function(i) {
    list(
      symbol = df$symbol[i],
      range = to_scip_range(df$line1[i], df$col1[i], df$line2[i], df$col2[i]),
      role = df$role[i]
    )
  })
}

#' Convert a parse-data range (1-based) into SCIP range coordinates (0-based).
#'
#' R parse data uses 1-based line/column coordinates with an inclusive end
#' column. SCIP expects 0-based line/column with an exclusive end column, so
#' line1/col1/line2 are shifted by -1 while col2 is preserved.
#'
#' @param line1 Start line (1-based).
#' @param col1 Start column (1-based).
#' @param line2 End line (1-based).
#' @param col2 End column (1-based inclusive).
#' @return Integer vector `c(start_line, start_col, end_line, end_col)` for SCIP.
#' @keywords internal
to_scip_range <- function(line1, col1, line2, col2) {
  c(
    as.integer(line1) - 1L,
    as.integer(col1) - 1L,
    as.integer(line2) - 1L,
    as.integer(col2)
  )
}
