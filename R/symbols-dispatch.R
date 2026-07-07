# Dynamic dispatch call-edge extraction.
# Detects Shiny, base-R, and C-style dispatch patterns and resolves their
# targets for SCIP relationships.
# Inventory: `extract_dynamic_call_edges()`, `extract_shiny_dispatch_edges()`,
# `extract_base_dispatch_edges()`, `extract_c_dispatch_edges()`.

#' @include parser.R
#' @include parse-tree-utils.R
#' @include parse-lookup.R
#' @include symbol-table.R
NULL

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
                                         relative_path, lookup = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  if (is.null(lookup)) {
    lookup <- build_lookup_index(data)
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
    full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

    if (is.na(full_call_id)) {
      next
    }

    # Get all descendants of the full call expression
    descendants <- get_descendants_lookup(full_call_id, lookup)

    # Extract target function name based on argument position:
    #   callModule(target, id, ...)  -> target is first arg
    #   moduleServer(id, target)     -> target is second arg
    target_info <- extract_shiny_dispatch_target_arg(
      call_text, full_call_id, descendants, data, lookup
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
extract_shiny_dispatch_target_arg <- function(call_text, full_call_id, descendants, data, lookup) {
  # Get direct children of the full call expression to find argument exprs
  children <- rows_by_parent_lookup(full_call_id, lookup)

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
  target_descendants <- get_descendants_lookup(target_expr$id, lookup)
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
                                        relative_path, lookup = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  if (is.null(lookup)) {
    lookup <- build_lookup_index(data)
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

    full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]
    if (is.na(full_call_id)) {
      full_call_id <- call_expr_id
    }

    descendants <- get_descendants_lookup(full_call_id, lookup)
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
  exclude_names <- c(
    "do.call", "match.fun", "args", "list",
    "what", "FUN", "ARGS"
  )
  symbol_rows <- symbol_rows[
    !symbol_rows$text %in% exclude_names, ,
    drop = FALSE
  ]

  if (nrow(symbol_rows) > 0) {
    return(list(name = symbol_rows$text[1], confidence = 0.9))
  }

  # Also check for SYMBOL_FUNCTION_CALL that is NOT the dispatch call itself
  # This handles nested cases like do.call(match.fun("func"), ...)
  inner_calls <- descendants[
    descendants$token == "SYMBOL_FUNCTION_CALL" &
      descendants$text != call_text, ,
    drop = FALSE
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
                                     relative_path, lookup = NULL) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  if (is.null(lookup)) {
    lookup <- build_lookup_index(data)
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

    full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]
    if (is.na(full_call_id)) {
      full_call_id <- call_expr_id
    }

    descendants <- get_descendants_lookup(full_call_id, lookup)
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
      confidence = 1.0, # String literal / backtick symbol -> always confident
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
  # Build a lightweight id/parent lookup once and share it across all three
  # extractors. This replaces their repeated `data[data$id == X, ]$parent` and
  # `get_descendants()` full-frame scans with O(1) / order-preserving lookups.
  lookup <- build_lookup_index(data)

  # Call all specialized extractors
  shiny_edges <- extract_shiny_dispatch_edges(
    data, tree_index, definitions_env, relative_path, lookup
  )

  base_edges <- extract_base_dispatch_edges(
    data, tree_index, definitions_env, relative_path, lookup
  )

  c_edges <- extract_c_dispatch_edges(
    data, tree_index, definitions_env, relative_path, lookup
  )

  # Combine all edges
  all_edges <- c(shiny_edges, base_edges, c_edges)

  all_edges
}
