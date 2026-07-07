# Diagnostics aggregation for parsing and indexing.
# Currently a no-op placeholder; diagnostics are not yet emitted.
# Inventory: `collect_diagnostics()`.

NULL

#' Collect diagnostics for a source file.
#'
#' This placeholder currently returns an empty list.
#'
#' @param file_info File descriptor list.
#' @param ast_module Parsed module data.
#' @param symbol_data Symbol resolution output.
#' @return An empty list.
#' @keywords internal
#' @examples
#' \dontrun{
#' collect_diagnostics(list(path = "script.R"), list(), list())
#' }
collect_diagnostics <- function(file_info, ast_module, symbol_data) {
  list()
}
