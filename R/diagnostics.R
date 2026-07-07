# Diagnostics aggregation for parsing and indexing.
# Normalizes warnings and lint output into SCIP-compatible diagnostic records.
# Inventory: `collect_diagnostics()`.

NULL

#' Collect diagnostics for a source file.
#'
#' @param file_info File descriptor list.
#' @param ast_module Parsed module data.
#' @param symbol_data Symbol resolution output.
#' @return A list of diagnostic entries with severity, message, and ranges.
#' @keywords internal
#' @examples
#' \dontrun{
#' collect_diagnostics(list(path = "script.R"), list(), list())
#' }
collect_diagnostics <- function(file_info, ast_module, symbol_data) {
  list()
}
