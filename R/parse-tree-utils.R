#' @include benchmark.R
#' @include parser.R
#' @useDynLib scipR, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

# This script provides parse tree navigation utilities for AST analysis.
# It centralizes tree traversal logic used across symbol detection modules
# (symbols.R, symbols-s3.R, symbols-r6.R) to avoid duplication and ensure
# consistent tree navigation patterns throughout the codebase.
#
# PERFORMANCE NOTE: For large files, use the indexed variants (_indexed suffix)
# which use pre-computed lookups instead of scanning the full data frame.
# Call build_tree_index() once per file, then pass the index to indexed functions.

#' Build a tree index for O(1) parent/child lookups.
#'
#' Pre-computes parent-child relationships from parse data to enable
#' fast tree navigation without repeated data frame scans.
#'
#' When Rcpp is available, uses C++ implementation for better performance.
#' Falls back to pure R implementation otherwise.
#'
#' Also pre-computes:
#' - enclosing_function: For each node, the ID of its innermost enclosing FUNCTION/OP-LAMBDA
#' - inside_function: Logical vector indicating if each node is inside a function body
#'
#' @param parse_data Parse data data.frame from getParseData().
#' @return A list with:
#'   - children: Named list mapping parent IDs to vectors of child IDs
#'   - parent: Named vector mapping node IDs to parent IDs
#'   - rows: Named list mapping node IDs to row indices in parse_data
#'   - data: Reference to the original parse_data
#'   - cpp_index: (optional) External pointer to C++ TreeIndex for fast lookups
#'   - enclosing_function: Integer vector of enclosing function IDs (NA for top-level)
#'   - inside_function: Logical vector indicating if node is inside a function
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' index <- build_tree_index(data)
#' children <- get_children_indexed(5, index)
#' }
build_tree_index <- function(parse_data) {
  bench_count("build_tree_index_call")

  if (is.null(parse_data) || nrow(parse_data) == 0) {
    empty_env <- new.env(hash = TRUE, parent = emptyenv())
    return(list(
      children = empty_env,
      parent = empty_env,
      rows = empty_env,
      by_line = empty_env,
      has_func_on_line = empty_env,
      data = parse_data,
      cpp_index = NULL,
      enclosing_function = integer(0),
      inside_function = logical(0),
      function_names = new.env(hash = TRUE, parent = emptyenv())
    ))
  }

  # Try to use C++ implementation for the index
  cpp_index <- NULL
  if (rcpp_available()) {
    cpp_index <- tryCatch(
      build_tree_index_cpp(parse_data$id, parse_data$parent, parse_data$line1),
      error = function(e) NULL
    )
  }

  ids_char <- as.character(parse_data$id)

  # Pre-compute children: parent_id -> vector of child ids (environment for O(1) lookup)
  children_list <- split(parse_data$id, parse_data$parent)
  children <- list2env(children_list, hash = TRUE, parent = emptyenv(), size = length(children_list))

  # Pre-compute parent lookup: node_id -> parent_id (environment for O(1) lookup)
  parent_vec <- stats::setNames(parse_data$parent, ids_char)
  parent <- list2env(as.list(parent_vec), hash = TRUE, parent = emptyenv(), size = nrow(parse_data))

  # Pre-compute row indices: node_id -> row index (environment for O(1) lookup)
  rows_vec <- stats::setNames(seq_len(nrow(parse_data)), ids_char)
  rows <- list2env(as.list(rows_vec), hash = TRUE, parent = emptyenv(), size = nrow(parse_data))

  # Pre-compute line index: line_number -> vector of row indices
  by_line_list <- split(seq_len(nrow(parse_data)), parse_data$line1)
  by_line <- list2env(by_line_list, hash = TRUE, parent = emptyenv(), size = length(by_line_list))

  # Pre-compute which lines contain function definitions (FUNCTION or OP-LAMBDA tokens)
  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_lines <- unique(parse_data$line1[parse_data$token %in% func_tokens])
  has_func_on_line <- new.env(hash = TRUE, parent = emptyenv(), size = max(length(func_lines), 1L))
  for (fl in as.character(func_lines)) {
    has_func_on_line[[fl]] <- TRUE
  }

  # Pre-compute enclosing function for each node (O(n) single pass)
  # This enables O(1) "is inside function" checks instead of O(depth) tree walks
  # Note: build_scope_index needs named vectors for vectorized indexing
  scope_result <- build_scope_index(parse_data, rows_vec, parent_vec)

  # Pre-compute function_node_id -> assigned_name map for scope encoding.
  # For each FUNCTION/OP-LAMBDA token, find the SYMBOL on the same line
  # before an assignment operator. This gives e.g. FUNCTION node_id -> "my_func".
  # Used by get_enclosing_function_name() for O(1) name lookups.
  function_names <- build_function_name_map(parse_data)

  list(
    children = children,
    parent = parent,
    rows = rows,
    by_line = by_line,
    has_func_on_line = has_func_on_line,
    data = parse_data,
    cpp_index = cpp_index,
    enclosing_function = scope_result$enclosing_function,
    inside_function = scope_result$inside_function,
    function_names = function_names
  )
}

#' Build scope index with enclosing function for each node.
#'
#' Uses position-based containment to determine which function (if any)
#' contains each node. The function scope is determined by the parent expr
#' of the FUNCTION/OP-LAMBDA token (which contains the full body), not the
#' token itself. Processes functions from largest to smallest so smaller
#' (inner) functions overwrite larger (outer) ones.
#'
#' @param parse_data Parse data data.frame.
#' @param rows Named vector mapping node IDs to row indices.
#' @param parent Named vector mapping node IDs to parent IDs.
#' @return List with enclosing_function (integer vector) and inside_function (logical vector).
#' @keywords internal
build_scope_index <- function(parse_data, rows, parent) {
  n <- nrow(parse_data)
  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_mask <- parse_data$token %in% func_tokens

  if (!any(func_mask)) {
    # No functions - everything is top-level
    return(list(
      enclosing_function = rep(NA_integer_, n),
      inside_function = rep(FALSE, n)
    ))
  }

  # Get the parent expr for each FUNCTION token - this gives the full function range
  # The FUNCTION token itself only covers the keyword, not the body
  func_ids <- parse_data$id[func_mask]
  func_parent_ids <- parent[as.character(func_ids)]

  # Get positions from parent expr nodes (which have full function body range)
  func_parent_rows <- rows[as.character(func_parent_ids)]
  valid_parents <- !is.na(func_parent_rows)

  if (!any(valid_parents)) {
    return(list(
      enclosing_function = rep(NA_integer_, n),
      inside_function = rep(FALSE, n)
    ))
  }

  func_parent_rows <- func_parent_rows[valid_parents]
  func_ids_valid <- func_ids[valid_parents]
  func_line1 <- parse_data$line1[func_parent_rows]
  func_col1 <- parse_data$col1[func_parent_rows]
  func_line2 <- parse_data$line2[func_parent_rows]
  func_col2 <- parse_data$col2[func_parent_rows]

  # Try C++ implementation first
  if (rcpp_available() && exists("build_scope_index_cpp", mode = "function")) {
    result <- tryCatch(
      {
        enclosing <- build_scope_index_cpp(
          parse_data$id,
          parse_data$line1, parse_data$col1,
          parse_data$line2, parse_data$col2,
          func_ids_valid, func_line1, func_col1, func_line2, func_col2
        )

        # inside_function: TRUE if enclosing is not NA AND this node is not the function itself
        inside <- !is.na(enclosing) & !func_mask

        list(
          enclosing_function = enclosing,
          inside_function = inside
        )
      },
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  # R fallback: position-based containment
  build_scope_index_r(parse_data, func_mask, func_ids_valid, func_line1, func_col1, func_line2, func_col2)
}

#' Build scope index (pure R implementation).
#'
#' @param parse_data Parse data data.frame.
#' @param func_mask Logical vector indicating FUNCTION/OP-LAMBDA nodes.
#' @param func_ids Integer vector of function node IDs.
#' @param func_line1 Integer vector of function scope start lines.
#' @param func_col1 Integer vector of function scope start columns.
#' @param func_line2 Integer vector of function scope end lines.
#' @param func_col2 Integer vector of function scope end columns.
#' @return List with enclosing_function and inside_function.
#' @keywords internal
build_scope_index_r <- function(parse_data, func_mask, func_ids, func_line1, func_col1, func_line2, func_col2) {
  n <- nrow(parse_data)

  # Build a data frame with function positions
  func_rows <- data.frame(
    id = func_ids,
    line1 = func_line1,
    col1 = func_col1,
    line2 = func_line2,
    col2 = func_col2
  )

  # Sort by size descending (outer functions first)
  # Size = span in lines * 10000 + span in columns (approximation)
  func_rows$size <- (func_rows$line2 - func_rows$line1) * 10000L +
    (func_rows$col2 - func_rows$col1)
  func_rows <- func_rows[order(-func_rows$size), , drop = FALSE]

  # Initialize: no enclosing function
  enclosing <- rep(NA_integer_, n)

  # Process from largest (outermost) to smallest (innermost)
  # Later (smaller) functions overwrite, giving innermost scope
  for (i in seq_len(nrow(func_rows))) {
    f <- func_rows[i, ]

    # Vectorized containment check
    # Node is contained if its range is within the function's range
    start_ok <- (parse_data$line1 > f$line1) |
      (parse_data$line1 == f$line1 & parse_data$col1 >= f$col1)
    end_ok <- (parse_data$line2 < f$line2) |
      (parse_data$line2 == f$line2 & parse_data$col2 <= f$col2)
    contained <- start_ok & end_ok

    enclosing[contained] <- f$id
  }

  # inside_function: TRUE if enclosing is not NA AND this node is not a FUNCTION/OP-LAMBDA itself
  inside <- !is.na(enclosing) & !func_mask

  list(
    enclosing_function = enclosing,
    inside_function = inside
  )
}

#' Check if Rcpp C++ functions are available
#'
#' @return TRUE if Rcpp C++ functions are compiled and loaded, FALSE otherwise
#' @keywords internal
rcpp_available <- function() {
  exists("build_tree_index_cpp", mode = "function") &&
    is.function(get("build_tree_index_cpp", mode = "function"))
}

#' Get child IDs using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the parent node.
#' @param index Tree index from build_tree_index().
#' @return Integer vector of child node IDs.
#' @keywords internal
get_children_indexed <- function(node_id, index) {
  get0(as.character(node_id), envir = index$children, ifnotfound = integer(0), inherits = FALSE)
}

#' Get parent ID using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the child node.
#' @param index Tree index from build_tree_index().
#' @return Integer parent ID, or NA if no parent.
#' @keywords internal
get_parent_id_indexed <- function(node_id, index) {
  get0(as.character(node_id), envir = index$parent, ifnotfound = NA_integer_, inherits = FALSE)
}

#' Get row index for a node using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the node.
#' @param index Tree index from build_tree_index().
#' @return Integer row index in parse_data, or NA if not found.
#' @keywords internal
get_row_indexed <- function(node_id, index) {
  get0(as.character(node_id), envir = index$rows, ifnotfound = NA_integer_, inherits = FALSE)
}

#' Check if a node is inside a function body using pre-computed index (O(1)).
#'
#' Uses the pre-computed inside_function vector from build_tree_index()
#' for O(1) lookup instead of O(depth) tree walking.
#'
#' @param node_id Numeric ID of the node to check.
#' @param index Tree index from build_tree_index().
#' @return Logical indicating if node is inside a function body.
#' @keywords internal
is_inside_function_indexed <- function(node_id, index) {
  row_idx <- get_row_indexed(node_id, index)
  if (is.na(row_idx)) {
    return(FALSE)
  }
  isTRUE(index$inside_function[row_idx])
}

#' Get the enclosing function ID for a node using pre-computed index (O(1)).
#'
#' Uses the pre-computed enclosing_function vector from build_tree_index()
#' for O(1) lookup instead of O(depth) tree walking.
#'
#' @param node_id Numeric ID of the node.
#' @param index Tree index from build_tree_index().
#' @return Integer ID of enclosing function, or NA if at top-level.
#' @keywords internal
get_enclosing_function_indexed <- function(node_id, index) {
  row_idx <- get_row_indexed(node_id, index)
  if (is.na(row_idx)) {
    return(NA_integer_)
  }
  index$enclosing_function[row_idx]
}

#' Build a map from FUNCTION/OP-LAMBDA node IDs to their assigned names.
#'
#' Scans parse data for assignment patterns like `my_func <- function(...)`.
#' For each FUNCTION or OP-LAMBDA token, finds the SYMBOL on the same line
#' before an assignment operator to determine the function's assigned name.
#'
#' This map enables O(1) lookup of function names by node ID, used by
#' get_enclosing_function_name() for scope encoding in local symbol IDs.
#'
#' @param parse_data Parse data data.frame from getParseData().
#' @return Environment mapping function node ID (as character) to function name string.
#'   Anonymous functions (no assignment found) are not included.
#' @keywords internal
build_function_name_map <- function(parse_data) {
  func_names_env <- new.env(hash = TRUE, parent = emptyenv())

  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_mask <- parse_data$token %in% func_tokens
  if (!any(func_mask)) {
    return(func_names_env)
  }

  # Pre-compute assignment positions: line -> list of (col, row_idx)
  assign_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN")
  assign_mask <- parse_data$token %in% assign_tokens
  assign_lines <- parse_data$line1[assign_mask]
  assign_cols <- parse_data$col1[assign_mask]

  # Build line -> min assignment col lookup
  assign_by_line <- new.env(hash = TRUE, parent = emptyenv())
  for (i in seq_along(assign_lines)) {
    lk <- as.character(assign_lines[i])
    existing <- assign_by_line[[lk]]
    if (is.null(existing) || assign_cols[i] < existing) {
      assign_by_line[[lk]] <- assign_cols[i]
    }
  }

  func_indices <- which(func_mask)
  for (fi in func_indices) {
    func_line <- parse_data$line1[fi]
    func_id <- parse_data$id[fi]
    func_col <- parse_data$col1[fi]
    line_key <- as.character(func_line)

    # Check if there's an assignment on this line
    assign_col <- assign_by_line[[line_key]]
    if (is.null(assign_col)) next

    # Skip if there's a SYMBOL_FUNCTION_CALL between the assignment and FUNCTION.
    # This filters out cases like: result <- lapply(1:10, function(x) ...)
    # where the FUNCTION is nested inside a call, not the direct assignment RHS.
    has_call_between <- any(
      parse_data$line1 == func_line &
        parse_data$token == "SYMBOL_FUNCTION_CALL" &
        parse_data$col1 > assign_col &
        parse_data$col1 < func_col
    )
    if (has_call_between) next

    # Find SYMBOL on same line, before the assignment operator
    same_line_mask <- parse_data$line1 == func_line &
      parse_data$token == "SYMBOL" &
      parse_data$col1 < assign_col
    candidates <- parse_data[same_line_mask, , drop = FALSE]

    if (nrow(candidates) == 0) next

    # Take the closest SYMBOL to the assignment (rightmost before it)
    best <- candidates[which.max(candidates$col1), ]
    func_names_env[[as.character(func_id)]] <- best$text
  }

  func_names_env
}

#' Get the name of the enclosing function for a given node.
#'
#' Uses the pre-computed enclosing_function vector and function_names map
#' from build_tree_index() for O(1) lookup. Returns the assigned name of
#' the innermost enclosing function, or NULL if the node is at top-level
#' or inside an anonymous function.
#'
#' @param node_id Numeric ID of the node.
#' @param index Tree index from build_tree_index().
#' @return Character function name, or NULL if not inside a named function.
#' @keywords internal
get_enclosing_function_name <- function(node_id, index) {
  enc_func_id <- get_enclosing_function_indexed(node_id, index)
  if (is.na(enc_func_id)) {
    return(NULL)
  }

  # Look up the function name from the pre-computed map
  func_name <- index$function_names[[as.character(enc_func_id)]]
  func_name # Returns NULL if not found (anonymous function)
}

#' Check if node has descendant of given type using DFS with early exit.
#'
#' Efficiently checks if any descendant node has one of the specified token types.
#' Uses depth-first search with early exit - stops as soon as a match is found.
#'
#' When C++ is available, uses optimized implementation with early exit.
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param index Tree index from build_tree_index().
#' @return Logical indicating if a descendant of the specified type exists.
#' @keywords internal
has_descendant_of_type_indexed <- function(node_id, token_types, index) {
  bench_count("has_descendant_check")

  # Use C++ implementation if available
  if (!is.null(index$cpp_index) && rcpp_available()) {
    result <- tryCatch(
      has_descendant_of_type_cpp(node_id, token_types, index$cpp_index, index$data$token),
      error = function(e) NULL
    )
    if (!is.null(result)) {
      return(result)
    }
  }

  # Fallback to R implementation
  has_descendant_of_type_indexed_r(node_id, token_types, index)
}

#' Check if node has descendant of given type (pure R implementation)
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param index Tree index from build_tree_index().
#' @return Logical indicating if a descendant of the specified type exists.
#' @keywords internal
has_descendant_of_type_indexed_r <- function(node_id, token_types, index) {
  stack <- get_children_indexed(node_id, index)

  while (length(stack) > 0) {
    bench_count("descendant_dfs_iteration")
    current <- stack[1]
    stack <- stack[-1]

    row_idx <- get_row_indexed(current, index)
    if (!is.na(row_idx) && index$data$token[row_idx] %in% token_types) {
      return(TRUE) # Early exit on first match
    }

    # Add children to front of stack (DFS)
    children <- get_children_indexed(current, index)
    if (length(children) > 0) {
      stack <- c(children, stack)
    }
  }

  FALSE
}

#' Get all descendant IDs using iterative traversal (O(n)).
#'
#' Collects all descendant node IDs. When C++ is available, uses pre-allocated
#' vectors with O(n) complexity. Falls back to R implementation otherwise.
#'
#' The C++ implementation eliminates the O(n^2) vector growth pattern that
#' occurs with repeated c() calls in the R implementation.
#'
#' @param node_id Numeric ID of the starting node.
#' @param index Tree index from build_tree_index().
#' @return Integer vector of all descendant node IDs.
#' @keywords internal
get_descendant_ids_indexed <- function(node_id, index) {
  # Use C++ implementation if available and index has cpp_index
  if (!is.null(index$cpp_index) && rcpp_available()) {
    return(tryCatch(
      get_descendant_ids_cpp(node_id, index$cpp_index),
      error = function(e) get_descendant_ids_indexed_r(node_id, index)
    ))
  }

  # Fallback to R implementation
  get_descendant_ids_indexed_r(node_id, index)
}

#' Get all descendant IDs (pure R implementation)
#'
#' This is the fallback R implementation. Note: This has O(n^2) complexity
#' due to vector growth with c(). Use the C++ version for large trees.
#'
#' @param node_id Numeric ID of the starting node.
#' @param index Tree index from build_tree_index().
#' @return Integer vector of all descendant node IDs.
#' @keywords internal
get_descendant_ids_indexed_r <- function(node_id, index) {
  result <- integer(0)
  stack <- get_children_indexed(node_id, index)

  while (length(stack) > 0) {
    current <- stack[1]
    stack <- stack[-1]
    result <- c(result, current)

    children <- get_children_indexed(current, index)
    if (length(children) > 0) {
      stack <- c(children, stack)
    }
  }

  result
}

#' Get all descendants as data frame using indexed traversal.
#'
#' Returns descendants as a data frame, but uses efficient indexed traversal
#' internally. Useful when you need the full row data, not just IDs.
#'
#' @param node_id Numeric ID of the starting node.
#' @param index Tree index from build_tree_index().
#' @return Data.frame subset containing all descendants.
#' @keywords internal
get_descendants_indexed <- function(node_id, index) {
  ids <- get_descendant_ids_indexed(node_id, index)
  if (length(ids) == 0) {
    return(index$data[integer(0), , drop = FALSE])
  }
  # Use row indices for fast subsetting
  row_indices <- vapply(ids, function(id) get_row_indexed(id, index), integer(1))
  row_indices <- row_indices[!is.na(row_indices)]
  index$data[row_indices, , drop = FALSE]
}

#' Find first ancestor of a given token type using pre-computed index (O(depth)).
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param index Tree index from build_tree_index().
#' @return Data.frame row containing the ancestor node, or empty data.frame if not found.
#' @keywords internal
find_ancestor_of_type_indexed <- function(node_id, token_types, index) {
  current_id <- node_id

  while (!is.na(current_id)) {
    row_idx <- get_row_indexed(current_id, index)
    if (is.na(row_idx)) break

    if (index$data$token[row_idx] %in% token_types) {
      return(index$data[row_idx, , drop = FALSE])
    }

    current_id <- get_parent_id_indexed(current_id, index)
  }

  index$data[integer(0), , drop = FALSE] # Not found
}

#' Find nodes on the same line using pre-computed line index (O(1) line lookup).
#'
#' @param node_id Numeric ID of the reference node.
#' @param index Tree index from build_tree_index().
#' @param token_types Optional character vector of token types to filter by.
#' @param position Optional: "before", "after", or NULL for all nodes on line.
#' @return Data.frame subset containing nodes on the same line.
#' @keywords internal
find_on_same_line_indexed <- function(node_id, index, token_types = NULL, position = NULL) {
  row_idx <- get_row_indexed(node_id, index)
  if (is.na(row_idx)) {
    return(index$data[integer(0), , drop = FALSE])
  }

  line <- index$data$line1[row_idx]
  col <- index$data$col1[row_idx]

  # O(1) lookup of row indices on this line
  line_row_indices <- get0(as.character(line), envir = index$by_line, ifnotfound = NULL, inherits = FALSE)
  if (is.null(line_row_indices) || length(line_row_indices) == 0) {
    return(index$data[integer(0), , drop = FALSE])
  }

  # Get subset of data for this line
  same_line <- index$data[line_row_indices, , drop = FALSE]

  # Filter by token type if specified
  if (!is.null(token_types)) {
    same_line <- same_line[same_line$token %in% token_types, , drop = FALSE]
  }

  # Filter by position if specified
  if (!is.null(position)) {
    if (position == "before") {
      same_line <- same_line[same_line$col1 < col, , drop = FALSE]
    } else if (position == "after") {
      same_line <- same_line[same_line$col1 > col, , drop = FALSE]
    }
  }

  # Exclude the node itself
  same_line[same_line$id != node_id, , drop = FALSE]
}
