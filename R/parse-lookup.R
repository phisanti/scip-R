# parse-lookup.R — O(1) per-file id/parent lookup index over parse_data.
# Builds `by_id` and `by_parent` row-index maps once per file; drop-in,
# order-preserving replacements for linear data[data$col==x,] scans.

#' Build a lightweight id/parent lookup index for a parse-data frame.
#'
#' Pre-computes two O(1) maps over the parse data:
#'   - by_id:     node id  -> row index in `data`
#'   - by_parent: parent id -> integer vector of child row indices (in row order)
#'
#' This is intentionally lighter than build_tree_index() (no C++ pointer, no
#' scope index). It exists solely to convert linear data-frame scans in the
#' symbol extraction hot path into indexed lookups without changing output.
#'
#' @param data Parse data data.frame from getParseData().
#' @return A list with `by_id` and `by_parent` environments, plus `data`.
#' @keywords internal
build_lookup_index <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    empty_env <- new.env(hash = TRUE, parent = emptyenv())
    return(list(by_id = empty_env, by_parent = empty_env, data = data))
  }

  n <- nrow(data)
  rows <- seq_len(n)

  # node id -> row index (ids are unique, so each maps to a single row)
  by_id_list <- split(rows, data$id)
  by_id <- list2env(by_id_list, hash = TRUE, parent = emptyenv(), size = length(by_id_list))

  # parent id -> child row indices, preserving parse_data row order
  by_parent_list <- split(rows, data$parent)
  by_parent <- list2env(by_parent_list, hash = TRUE, parent = emptyenv(), size = length(by_parent_list))

  list(by_id = by_id, by_parent = by_parent, data = data)
}

#' Look up the row(s) for a node id using a lookup index.
#'
#' Drop-in replacement for `data[data$id == node_id, , drop = FALSE]` that
#' preserves row order and returns an empty (0-row) subset when absent.
#'
#' @param node_id Node id to look up.
#' @param lookup Lookup index from build_lookup_index().
#' @return Data.frame subset (0 or more rows) for the given id.
#' @keywords internal
rows_by_id_lookup <- function(node_id, lookup) {
  row_idx <- get0(as.character(node_id), envir = lookup$by_id, ifnotfound = NULL, inherits = FALSE)
  if (is.null(row_idx)) {
    return(lookup$data[integer(0), , drop = FALSE])
  }
  lookup$data[row_idx, , drop = FALSE]
}

#' Look up child rows for a parent id using a lookup index.
#'
#' Drop-in replacement for `data[data$parent == parent_id, , drop = FALSE]`.
#' Child row indices are stored in parse_data row order, so the returned subset
#' matches the linear scan exactly.
#'
#' @param parent_id Parent id whose children to fetch.
#' @param lookup Lookup index from build_lookup_index().
#' @return Data.frame subset containing immediate children (in row order).
#' @keywords internal
rows_by_parent_lookup <- function(parent_id, lookup) {
  child_rows <- get0(as.character(parent_id), envir = lookup$by_parent, ifnotfound = NULL, inherits = FALSE)
  if (is.null(child_rows)) {
    return(lookup$data[integer(0), , drop = FALSE])
  }
  lookup$data[child_rows, , drop = FALSE]
}

#' Get all descendants as a data frame using a lookup index.
#'
#' Reproduces the exact traversal order of the legacy `get_descendants()`
#' (breadth-first by parent, each sibling block in parse_data row order) while
#' using O(1) `by_parent` lookups instead of repeated full-frame scans. The
#' returned rows are byte-identical to `get_descendants(node_id, data)`.
#'
#' @param node_id Numeric ID of the starting node.
#' @param lookup Lookup index from build_lookup_index().
#' @return Data.frame subset containing all descendants (legacy order).
#' @keywords internal
get_descendants_lookup <- function(node_id, lookup) {
  data <- lookup$data

  # BFS over parents, collecting child row-index blocks in dequeue order.
  # This mirrors get_descendants(): each dequeued node contributes its
  # children (in row order) as one contiguous block, then those children
  # are enqueued for further traversal.
  row_blocks <- list()
  queue <- list(node_id)

  while (length(queue) > 0) {
    current_id <- queue[[1]]
    queue <- queue[-1]

    child_rows <- get0(as.character(current_id), envir = lookup$by_parent, ifnotfound = NULL, inherits = FALSE)
    if (!is.null(child_rows) && length(child_rows) > 0) {
      row_blocks[[length(row_blocks) + 1]] <- child_rows
      for (cid in data$id[child_rows]) {
        queue[[length(queue) + 1]] <- cid
      }
    }
  }

  if (length(row_blocks) == 0) {
    return(data[integer(0), , drop = FALSE])
  }

  data[unlist(row_blocks, use.names = FALSE), , drop = FALSE]
}
