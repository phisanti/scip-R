#' @include parser.R
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
#' @param parse_data Parse data data.frame from getParseData().
#' @return A list with:
#'   - children: Named list mapping parent IDs to vectors of child IDs
#'   - parent: Named vector mapping node IDs to parent IDs
#'   - rows: Named list mapping node IDs to row indices in parse_data
#'   - data: Reference to the original parse_data
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' index <- build_tree_index(data)
#' children <- get_children_indexed(5, index)
#' }
build_tree_index <- function(parse_data) {
  if (is.null(parse_data) || nrow(parse_data) == 0) {
    return(list(
      children = list(),
      parent = integer(0),
      rows = list(),
      by_line = list(),
      data = parse_data
    ))
  }

  # Pre-compute children: parent_id -> vector of child ids
  children <- split(parse_data$id, parse_data$parent)

  # Pre-compute parent lookup: node_id -> parent_id
  parent <- setNames(parse_data$parent, as.character(parse_data$id))

  # Pre-compute row indices: node_id -> row index in parse_data
  rows <- setNames(seq_len(nrow(parse_data)), as.character(parse_data$id))

  # Pre-compute line index: line_number -> vector of row indices
  by_line <- split(seq_len(nrow(parse_data)), parse_data$line1)

  # Pre-compute which lines contain function definitions (FUNCTION or OP-LAMBDA tokens)
  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_lines <- unique(parse_data$line1[parse_data$token %in% func_tokens])
  has_func_on_line <- setNames(rep(TRUE, length(func_lines)), as.character(func_lines))

  list(
    children = children,
    parent = parent,
    rows = rows,
    by_line = by_line,
    has_func_on_line = has_func_on_line,
    data = parse_data
  )
}

#' Get child IDs using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the parent node.
#' @param index Tree index from build_tree_index().
#' @return Integer vector of child node IDs.
#' @keywords internal
get_children_indexed <- function(node_id, index) {
  index$children[[as.character(node_id)]] %||% integer(0)
}

#' Get parent ID using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the child node.
#' @param index Tree index from build_tree_index().
#' @return Integer parent ID, or NA if no parent.
#' @keywords internal
get_parent_id_indexed <- function(node_id, index) {
  index$parent[[as.character(node_id)]]
}

#' Get row index for a node using pre-computed index (O(1)).
#'
#' @param node_id Numeric ID of the node.
#' @param index Tree index from build_tree_index().
#' @return Integer row index in parse_data, or NA if not found.
#' @keywords internal
get_row_indexed <- function(node_id, index) {
  index$rows[[as.character(node_id)]]
}

#' Check if node has descendant of given type using DFS with early exit.
#'
#' Efficiently checks if any descendant node has one of the specified token types.
#' Uses depth-first search with early exit - stops as soon as a match is found.
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param index Tree index from build_tree_index().
#' @return Logical indicating if a descendant of the specified type exists.
#' @keywords internal
has_descendant_of_type_indexed <- function(node_id, token_types, index) {
  stack <- get_children_indexed(node_id, index)

  while (length(stack) > 0) {
    current <- stack[1]
    stack <- stack[-1]

    row_idx <- get_row_indexed(current, index)
    if (!is.na(row_idx) && index$data$token[row_idx] %in% token_types) {
      return(TRUE)  # Early exit on first match
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
#' Collects all descendant node IDs without the O(n^2) rbind overhead.
#' Uses a worklist algorithm with pre-allocated output.
#'
#' @param node_id Numeric ID of the starting node.
#' @param index Tree index from build_tree_index().
#' @return Integer vector of all descendant node IDs.
#' @keywords internal
get_descendant_ids_indexed <- function(node_id, index) {
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

# ============================================================================
# Legacy functions (O(n) scans) - kept for backwards compatibility
# Prefer indexed variants for performance-critical code paths
# ============================================================================

#' Get all child nodes of a given node.
#'
#' Returns immediate children only (not descendants).
#'
#' @param node_id Numeric ID of the parent node.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame subset containing immediate children.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' children <- get_children(5, data)
#' }
get_children <- function(node_id, parse_data) {
  parse_data[parse_data$parent == node_id, , drop = FALSE]
}

#' Get all descendant nodes recursively.
#'
#' Collects all child nodes and their descendants using iterative traversal.
#' Uses list accumulation + single rbind to avoid O(n^2) memory allocation.
#'
#' @param node_id Numeric ID of the starting node.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame subset containing all descendants.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' descendants <- get_descendants(5, data)
#' }
get_descendants <- function(node_id, parse_data) {
  # Use iterative BFS with list accumulation to avoid O(n^2) rbind
  result_list <- list()
  stack <- list(node_id)

  while (length(stack) > 0) {
    current_id <- stack[[1]]
    stack <- stack[-1]

    children <- parse_data[parse_data$parent == current_id, , drop = FALSE]
    if (nrow(children) > 0) {
      result_list[[length(result_list) + 1]] <- children
      # Add child IDs to stack for further traversal
      for (cid in children$id) {
        stack[[length(stack) + 1]] <- cid
      }
    }
  }

  if (length(result_list) == 0) {
    return(parse_data[integer(0), , drop = FALSE])
  }

  # Single rbind at the end - O(n) instead of O(n^2)
  do.call(rbind, result_list)
}

#' Get parent node of a given node.
#'
#' Returns the immediate parent node in the parse tree.
#'
#' @param node_id Numeric ID of the child node.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame row containing the parent node, or empty data.frame if no parent.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' parent <- get_parent(5, data)
#' }
get_parent <- function(node_id, parse_data) {
  node <- parse_data[parse_data$id == node_id, , drop = FALSE]
  if (nrow(node) == 0 || is.na(node$parent[1])) {
    return(data.frame())
  }
  parent_id <- node$parent[1]
  parse_data[parse_data$id == parent_id, , drop = FALSE]
}

#' Get all siblings of a node (nodes with same parent).
#'
#' Returns all nodes that share the same parent, excluding the node itself.
#'
#' @param node_id Numeric ID of the node.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame subset containing sibling nodes.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' siblings <- get_siblings(5, data)
#' }
get_siblings <- function(node_id, parse_data) {
  node <- parse_data[parse_data$id == node_id, , drop = FALSE]
  if (nrow(node) == 0) {
    return(data.frame())
  }

  parent_id <- node$parent[1]
  if (is.na(parent_id)) {
    return(data.frame())
  }

  siblings <- parse_data[parse_data$parent == parent_id, , drop = FALSE]
  siblings[siblings$id != node_id, , drop = FALSE]
}

#' Find first ancestor of a given token type.
#'
#' Traverses up the parse tree until finding a node with one of the specified
#' token types. Returns the first match or empty data.frame if not found.
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame row containing the ancestor node, or empty data.frame if not found.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' func_ancestor <- find_ancestor_of_type(5, c("FUNCTION", "OP-LAMBDA"), data)
#' }
find_ancestor_of_type <- function(node_id, token_types, parse_data) {
  current <- parse_data[parse_data$id == node_id, , drop = FALSE]

  while (nrow(current) > 0) {
    if (current$token[1] %in% token_types) {
      return(current)
    }
    current <- get_parent(current$id[1], parse_data)
  }

  data.frame()  # Not found
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

  index$data[integer(0), , drop = FALSE]  # Not found
}

#' Check if node has descendant of given type.
#'
#' Efficiently checks if any descendant node has one of the specified token types.
#' Useful for detecting patterns like "does this assignment contain a FUNCTION token?"
#'
#' @param node_id Numeric ID of the starting node.
#' @param token_types Character vector of token types to search for.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Logical indicating if a descendant of the specified type exists.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' is_func_def <- has_descendant_of_type(5, c("FUNCTION", "OP-LAMBDA"), data)
#' }
has_descendant_of_type <- function(node_id, token_types, parse_data) {
  descendants <- get_descendants(node_id, parse_data)
  if (nrow(descendants) == 0) {
    return(FALSE)
  }
  any(descendants$token %in% token_types)
}

#' Find nodes on the same line as a given node.
#'
#' Returns all nodes that appear on the same line, optionally filtered by token type
#' and position (before or after the target node).
#'
#' @param node_id Numeric ID of the reference node.
#' @param parse_data Parse data data.frame from getParseData().
#' @param token_types Optional character vector of token types to filter by.
#' @param position Optional: "before", "after", or NULL for all nodes on line.
#' @return Data.frame subset containing nodes on the same line.
#' @keywords internal
#' @examples
#' \dontrun{
#' data <- getParseData(parse("script.R", keep.source = TRUE))
#' # Find assignments after this symbol on same line
#' assignments <- find_on_same_line(5, data, c("LEFT_ASSIGN", "EQ_ASSIGN"), "after")
#' }
find_on_same_line <- function(node_id, parse_data, token_types = NULL, position = NULL) {
  node <- parse_data[parse_data$id == node_id, , drop = FALSE]
  if (nrow(node) == 0) {
    return(data.frame())
  }

  line <- node$line1[1]
  col <- node$col1[1]

  # Filter by line
  same_line <- parse_data[parse_data$line1 == line, , drop = FALSE]

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
  line_row_indices <- index$by_line[[as.character(line)]]
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

#' Get the expression node containing an assignment.
#'
#' Given an assignment operator node (LEFT_ASSIGN, RIGHT_ASSIGN, EQ_ASSIGN),
#' returns the parent expression node that contains the full assignment.
#'
#' @param assignment_id Numeric ID of the assignment operator node.
#' @param parse_data Parse data data.frame from getParseData().
#' @return Data.frame row containing the expression node, or empty data.frame.
#' @keywords internal
get_assignment_expr <- function(assignment_id, parse_data) {
  # Assignment operators are typically within an expr node
  find_ancestor_of_type(assignment_id, "expr", parse_data)
}
