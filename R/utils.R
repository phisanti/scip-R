NULL

# This script provides utility functions shared across the scip-r package.
# It centralizes common helper operators and functions to avoid duplication.

#' Default operator for NULL or empty values.
#'
#' The `%||%` operator returns the right-hand side if the left-hand side is
#' NULL or empty. This consolidates behavior across different contexts
#' (character, list, etc.).
#'
#' @param lhs Left-hand side value to check.
#' @param rhs Right-hand side default value to return if lhs is NULL/empty.
#' @return Either lhs (if non-NULL and non-empty) or rhs.
#' @keywords internal
#' @name or_op
#' @examples
#' \dontrun{
#' NULL %||% "default"  # returns "default"
#' "value" %||% "default"  # returns "value"
#' character() %||% "default"  # returns "default"
#' list() %||% list(a = 1)  # returns list(a = 1)
#' }
`%||%` <- function(lhs, rhs) {
  if (is.null(lhs)) {
    return(rhs)
  }
  if (is.character(lhs) && length(lhs) == 0) {
    return(rhs)
  }
  if (is.list(lhs) && length(lhs) == 0) {
    return(rhs)
  }
  lhs
}

#' Escape an identifier for SCIP symbol strings.
#'
#' Per the SCIP spec, valid identifier characters are `_`, `+`, `-`, `$`,
#' ASCII letters, and digits. Names containing anything else (notably `.`
#' in R) must be backtick-quoted. Internal backticks are escaped by doubling.
#'
#' @param name Character scalar: the raw identifier name.
#' @return The name unchanged if it only contains identifier characters,
#'   or backtick-quoted otherwise.
#' @keywords internal
escape_scip_identifier <- function(name) {
  if (grepl("[^A-Za-z0-9_+$-]", name)) {
    escaped <- gsub("`", "``", name, fixed = TRUE)
    return(paste0("`", escaped, "`"))
  }
  name
}
