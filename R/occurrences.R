#' @include utils.R
#' @include symbols.R
#' @include parse-tree-utils.R
NULL

# This script constructs occurrence records and document payloads for SCIP.
# It consumes symbol tables to map definitions and references with ranges.
# Includes enclosing range computation for AST context.

#' Map occurrences from resolved symbols and AST module.
#'
#' Processes occurrences to add enclosing range information for each symbol.
#' For definitions, the enclosing range spans the entire definition (assignment
#' or function body). For references, it spans the parent expression/statement.
#'
#' @param symbol_data Symbol resolution output.
#' @param ast_module Parsed module data.
#' @return A list of occurrence entries with symbol identifiers, ranges, and enclosing ranges.
#' @keywords internal
map_occurrences <- function(symbol_data, ast_module) {
  occurrences <- symbol_data$occurrences %||% list()

  if (length(occurrences) == 0) {
    return(list())
  }

  parse_data <- ast_module$parse_data
  if (is.null(parse_data) || nrow(parse_data) == 0) {
    return(occurrences)
  }

  # Build tree index once for O(1) lookups
  tree_index <- build_tree_index(parse_data)

  # Process each occurrence to compute enclosing range
  lapply(occurrences, function(occ) {
    enclosing <- compute_enclosing_range(occ, parse_data, tree_index)
    if (!is.null(enclosing)) {
      occ$enclosing_range <- enclosing
    }
    occ
  })
}

#' Compute the enclosing range for an occurrence.
#'
#' @param occurrence An occurrence list with symbol, range, and role fields.
#' @param parse_data Parse data data.frame from getParseData().
#' @param tree_index Tree index from build_tree_index() for O(1) lookups.
#' @return A 4-element integer vector or NULL if no enclosing range can be determined.
#' @keywords internal
compute_enclosing_range <- function(occurrence, parse_data, tree_index) {
  range <- occurrence$range
  role <- occurrence$role
  start_line <- range[1] + 1
  start_col <- range[2] + 1
  end_line <- range[3] + 1
  end_col <- range[4]

  candidates <- parse_data[
    parse_data$line1 == start_line &
    parse_data$col1 == start_col &
    parse_data$line2 == end_line &
    parse_data$col2 == end_col,
    , drop = FALSE
  ]

  if (nrow(candidates) == 0) {
    candidates <- parse_data[
      parse_data$line1 == start_line &
      parse_data$col1 == start_col,
      , drop = FALSE
    ]
  }

  if (nrow(candidates) == 0) {
    return(NULL)
  }

  node_id <- candidates$id[1]

  if (identical(role, "definition")) {
    enclosing_node <- find_enclosing_definition(node_id, parse_data, tree_index)
  } else {
    enclosing_node <- find_enclosing_expression(node_id, parse_data, tree_index)
  }

  if (is.null(enclosing_node) || nrow(enclosing_node) == 0) {
    return(NULL)
  }

  c(
    enclosing_node$line1[1] - 1,
    enclosing_node$col1[1] - 1,
    enclosing_node$line2[1] - 1,
    enclosing_node$col2[1]
  )
}

#' Find the enclosing definition node for a symbol.
#' @param node_id Numeric ID of the symbol node.
#' @param parse_data Parse data data.frame.
#' @param tree_index Tree index for O(1) lookups.
#' @return Data.frame row of the enclosing definition node, or empty data.frame.
#' @keywords internal
find_enclosing_definition <- function(node_id, parse_data, tree_index) {
  current_id <- node_id
  assignment_expr <- NULL

  while (!is.na(current_id)) {
    parent_id <- get_parent_id_indexed(current_id, tree_index)
    if (is.na(parent_id)) break
    row_idx <- get_row_indexed(parent_id, tree_index)
    if (is.na(row_idx)) break
    parent_token <- parse_data$token[row_idx]

    if (parent_token == "expr") {
      children_ids <- get_children_indexed(parent_id, tree_index)
      for (child_id in children_ids) {
        child_row_idx <- get_row_indexed(child_id, tree_index)
        if (!is.na(child_row_idx)) {
          child_token <- parse_data$token[child_row_idx]
          if (child_token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")) {
            assignment_expr <- parse_data[row_idx, , drop = FALSE]
            break
          }
        }
      }
    }
    if (parent_token == "forcond") {
      for_expr <- find_ancestor_of_type_indexed(parent_id, "FOR", tree_index)
      if (nrow(for_expr) > 0) {
        return(find_ancestor_of_type_indexed(for_expr$id[1], "expr", tree_index))
      }
    }
    if (!is.null(assignment_expr)) break
    current_id <- parent_id
  }

  if (!is.null(assignment_expr) && nrow(assignment_expr) > 0) {
    return(assignment_expr)
  }
  find_ancestor_of_type_indexed(node_id, "expr", tree_index)
}

#' Find the enclosing expression node for a reference.
#' @param node_id Numeric ID of the symbol node.
#' @param parse_data Parse data data.frame.
#' @param tree_index Tree index for O(1) lookups.
#' @return Data.frame row of the enclosing expression node, or empty data.frame.
#' @keywords internal
find_enclosing_expression <- function(node_id, parse_data, tree_index) {
  current_id <- node_id
  best_expr <- NULL

  while (!is.na(current_id)) {
    parent_id <- get_parent_id_indexed(current_id, tree_index)
    if (is.na(parent_id)) break
    row_idx <- get_row_indexed(parent_id, tree_index)
    if (is.na(row_idx)) break
    parent_token <- parse_data$token[row_idx]

    if (parent_token == "expr") {
      children_ids <- get_children_indexed(parent_id, tree_index)
      if (length(children_ids) > 1) {
        best_expr <- parse_data[row_idx, , drop = FALSE]
        break
      } else if (length(children_ids) == 1) {
        current_id <- parent_id
        next
      } else {
        break
      }
    } else if (parent_token %in% c("IF", "WHILE", "FOR", "REPEAT", "FUNCTION")) {
      enclosing_expr <- find_ancestor_of_type_indexed(parent_id, "expr", tree_index)
      if (nrow(enclosing_expr) > 0) {
        return(enclosing_expr)
      }
      break
    }
    current_id <- parent_id
  }

  if (!is.null(best_expr) && nrow(best_expr) > 0) {
    return(best_expr)
  }
  find_ancestor_of_type_indexed(node_id, "expr", tree_index)
}

#' Build a SCIP document message structure.
#'
#' @param file_info Source file descriptor list.
#' @param occurrences List of occurrences returned by map_occurrences.
#' @param symbol_data Symbol resolution output.
#' @return A list representing a document suitable for serialization.
#' @keywords internal
build_document <- function(file_info, occurrences, symbol_data) {
  list(
    language = "R",
    relative_path = file_info$relative_path,
    occurrences = occurrences,
    symbols = symbol_data$symbols
  )
}
