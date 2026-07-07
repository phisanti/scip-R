# Shared utility helpers for scip-r.
# Provides package-wide operators and small helpers used across modules.
# Inventory: `%||%`, `escape_scip_identifier()`, `scip_r_version()`.

NULL

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
#' NULL %||% "default" # returns "default"
#' "value" %||% "default" # returns "value"
#' character() %||% "default" # returns "default"
#' list() %||% list(a = 1) # returns list(a = 1)
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

#' Read this package's version from DESCRIPTION.
#'
#' @return Character scalar package version.
#' @keywords internal
scip_r_version <- function() {
  desc_path <- system.file("DESCRIPTION", package = "scipR")
  if (nzchar(desc_path) && file.exists(desc_path)) {
    desc <- read.dcf(desc_path, fields = "Version")
    version <- desc[1, "Version"]
    if (!is.na(version) && nzchar(version)) {
      return(version)
    }
  }

  pkg_desc <- utils::packageDescription("scipR", fields = "Version")
  if (!is.na(pkg_desc) && nzchar(pkg_desc)) {
    return(pkg_desc)
  }

  local_desc <- file.path(getwd(), "DESCRIPTION")
  if (file.exists(local_desc)) {
    desc <- read.dcf(local_desc, fields = c("Package", "Version"))
    if (identical(desc[1, "Package"], "scipR")) {
      version <- desc[1, "Version"]
      if (!is.na(version) && nzchar(version)) {
        return(version)
      }
    }
  }

  "0.0.0-dev"
}
