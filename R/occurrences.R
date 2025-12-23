#' @include utils.R
#' @include symbols.R
NULL

# This script constructs occurrence records and document payloads for SCIP.
# It consumes symbol tables to map definitions and references with ranges.
# Future versions should add precise role detection and enclosing range logic.

#' Map occurrences from resolved symbols and AST module.
#'
#' @param symbol_data Symbol resolution output.
#' @param ast_module Parsed module data.
#' @return A list of occurrence entries with symbol identifiers and ranges.
#' @export
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' sym <- resolve_symbols(ast, analyze_workspace(tempdir()))
#' map_occurrences(sym, ast)
#' }
map_occurrences <- function(symbol_data, ast_module) {
  symbol_data$occurrences %||% list()
}

#' Build a SCIP document message structure.
#'
#' @param file_info Source file descriptor list.
#' @param occurrences List of occurrences returned by `map_occurrences()`.
#' @param symbol_data Symbol resolution output.
#' @return A list representing a document suitable for serialization.
#' @export
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' sym <- resolve_symbols(ast, analyze_workspace(tempdir()))
#' occ <- map_occurrences(sym, ast)
#' build_document(list(path = "script.R", relative_path = "script.R"), occ, sym)
#' }
build_document <- function(file_info, occurrences, symbol_data) {
  list(
    language = "R",
    relative_path = file_info$relative_path,
    occurrences = occurrences,
    symbols = symbol_data$symbols
  )
}
