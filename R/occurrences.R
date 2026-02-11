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
#' Uses vectorized `match()` for O(n+m) node lookup instead of per-occurrence
#' O(n) data.frame scans. This reduces the overall complexity from O(n*m) to
#' O(n+m) where n=occurrences and m=parse_data rows.
#'
#' @param symbol_data Symbol resolution output.
#' @param ast_module Parsed module data.
#' @param tree_index Optional pre-built tree index from build_tree_index().
#'   If NULL, one will be built. Passing a cached index avoids redundant work.
#' @return A list of occurrence entries with symbol identifiers, ranges, and enclosing ranges.
#' @keywords internal
map_occurrences <- function(symbol_data, ast_module, tree_index = NULL) {
  occurrences <- symbol_data$occurrences %||% list()

  if (length(occurrences) == 0) {
    return(list())
  }

  parse_data <- ast_module$parse_data

  n_occ <- length(occurrences)

  # Handle closure definitions first (they carry pre-computed ranges, no parse_data needed)
  closure_mask <- logical(n_occ)
  for (i in seq_len(n_occ)) {
    if (!is.null(occurrences[[i]]$closure_func_range)) {
      occurrences[[i]]$enclosing_range <- occurrences[[i]]$closure_func_range
      occurrences[[i]]$closure_func_range <- NULL
      closure_mask[i] <- TRUE
    }
  }

  # Early return if no parse_data or all occurrences are closures
  if (is.null(parse_data) || nrow(parse_data) == 0) {
    return(occurrences)
  }

  # Identify occurrences that need enclosing range computation
  idx_to_process <- which(!closure_mask)
  if (length(idx_to_process) == 0) {
    return(occurrences)
  }

  # Reuse tree index if provided, otherwise build once
  if (is.null(tree_index)) {
    tree_index <- build_tree_index(parse_data)
  }

  # Extract ranges as vectors for vectorized matching (0-based SCIP -> 1-based R)
  occ_line1 <- integer(length(idx_to_process))
  occ_col1 <- integer(length(idx_to_process))
  occ_line2 <- integer(length(idx_to_process))
  occ_col2 <- integer(length(idx_to_process))
  occ_roles <- character(length(idx_to_process))

  for (j in seq_along(idx_to_process)) {
    r <- occurrences[[idx_to_process[j]]]$range
    occ_line1[j] <- r[1] + 1L
    occ_col1[j] <- r[2] + 1L
    occ_line2[j] <- r[3] + 1L
    occ_col2[j] <- r[4]
    occ_roles[j] <- occurrences[[idx_to_process[j]]]$role %||% "reference"
  }

  # Build position keys for vectorized matching via match() — O(n+m)
  pd_keys <- paste(parse_data$line1, parse_data$col1, parse_data$line2, parse_data$col2, sep = ":")
  occ_keys <- paste(occ_line1, occ_col1, occ_line2, occ_col2, sep = ":")

  # Single match() call replaces n per-occurrence data.frame scans
  match_idx <- match(occ_keys, pd_keys)

  # Fallback for unmatched: try (line1, col1) only
  unmatched <- is.na(match_idx)
  if (any(unmatched)) {
    pd_keys_partial <- paste(parse_data$line1, parse_data$col1, sep = ":")
    occ_keys_partial <- paste(occ_line1[unmatched], occ_col1[unmatched], sep = ":")
    match_idx[unmatched] <- match(occ_keys_partial, pd_keys_partial)
  }

  # Compute enclosing ranges using tree walks (O(depth) per occurrence)
  for (j in seq_along(idx_to_process)) {
    m <- match_idx[j]
    if (is.na(m)) next

    node_id <- parse_data$id[m]
    enclosing_node <- if (identical(occ_roles[j], "definition")) {
      find_enclosing_definition(node_id, parse_data, tree_index)
    } else {
      find_enclosing_expression(node_id, parse_data, tree_index)
    }

    if (!is.null(enclosing_node) && nrow(enclosing_node) > 0) {
      occurrences[[idx_to_process[j]]]$enclosing_range <- c(
        enclosing_node$line1[1] - 1L,
        enclosing_node$col1[1] - 1L,
        enclosing_node$line2[1] - 1L,
        enclosing_node$col2[1]
      )
    }
  }

  occurrences
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
          # EQ_SUB means this is a named list element (name = value)
          # The enclosing range should be the parent expr containing the value
          if (child_token == "EQ_SUB") {
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
