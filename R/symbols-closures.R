# LIST CLOSURE DEFINITIONS
# Split from symbols.R: detection of named function closures assigned as list
# elements at top level. Move-only; no logic change.

#' @include parser.R
#' @include parse-tree-utils.R
#' @include parse-lookup.R
NULL


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
    if (call_name %in% c(
      "R6Class", "setClass", "setRefClass", "setGeneric",
      "setMethod", "setValidity", "representation"
    )) {
      next
    }

    # Only handle list() calls
    if (call_name != "list") next

    # Find the SYMBOL_SUB before this EQ_SUB (the element name)
    sym_sub_rows <- siblings[
      siblings$token == "SYMBOL_SUB" &
        (siblings$line1 < eq_row$line1 |
          (siblings$line1 == eq_row$line1 & siblings$col1 < eq_row$col1)), ,
      drop = FALSE
    ]

    if (nrow(sym_sub_rows) == 0) next

    # Get the closest SYMBOL_SUB (last one before EQ_SUB)
    sym_sub_rows <- sym_sub_rows[order(sym_sub_rows$line1, sym_sub_rows$col1), , drop = FALSE]
    sym_sub <- sym_sub_rows[nrow(sym_sub_rows), ]

    # Find the expr after this EQ_SUB (the RHS value)
    rhs_expr_rows <- siblings[
      siblings$token == "expr" &
        (siblings$line1 > eq_row$line1 |
          (siblings$line1 == eq_row$line1 & siblings$col1 > eq_row$col1)), ,
      drop = FALSE
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
  if (length(children_ids) == 0) {
    return(NULL)
  }

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
    if (is.na(parent_id)) {
      return(NULL)
    }

    parent_row_idx <- get_row_indexed(parent_id, tree_index)
    if (is.na(parent_row_idx)) {
      return(NULL)
    }

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
