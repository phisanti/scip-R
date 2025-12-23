#' @include parser.R
NULL

# This script provides parse tree navigation utilities for AST analysis.
# It centralizes tree traversal logic used across symbol detection modules
# (symbols.R, symbols-s3.R, symbols-r6.R) to avoid duplication and ensure
# consistent tree navigation patterns throughout the codebase.

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
#' Recursively collects all child nodes and their descendants.
#' This is the canonical implementation used across all symbol detection modules.
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
  children <- get_children(node_id, parse_data)

  if (nrow(children) == 0) {
    return(children)
  }

  all_descendants <- children
  for (child_id in children$id) {
    child_descendants <- get_descendants(child_id, parse_data)
    if (nrow(child_descendants) > 0) {
      all_descendants <- rbind(all_descendants, child_descendants)
    }
  }

  all_descendants
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
