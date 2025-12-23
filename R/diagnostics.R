NULL

# This script aggregates diagnostics generated during parsing and analysis.
# It normalizes warnings and lint results into SCIP-compatible structures.
# Future implementations should integrate lintr and condition handling hooks.

#' Collect diagnostics for a source file.
#'
#' @param file_info File descriptor list.
#' @param ast_module Parsed module data.
#' @param symbol_data Symbol resolution output.
#' @return A list of diagnostic entries with severity, message, and ranges.
#' @export
#' @examples
#' collect_diagnostics(list(path = "script.R"), list(), list())
collect_diagnostics <- function(file_info, ast_module, symbol_data) {
  list()
}
