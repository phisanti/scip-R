#' @include files.R
#' @importFrom utils getParseData
NULL

# This script converts R source files into abstract syntax structures for indexing.
# It wraps base parsing capabilities and normalizes tokens for downstream passes.
# Future revisions should enrich the IR with scope-aware annotations.

#' Parse an R source file into an AST module representation.
#'
#' @param file_info List describing the source file (path, relative_path, etc.).
#' @return A list containing the parsed expressions, parse data, and source references.
#' @keywords internal
#' @examples
#' \dontrun{
#' file_info <- list(path = "script.R", relative_path = "script.R")
#' parse_file(file_info)
#' }
parse_file <- function(file_info) {
  path <- file_info$path
  exprs <- parse(path, keep.source = TRUE)
  parse_data <- getParseData(exprs)
  list(
    expressions = exprs,
    parse_data = parse_data,
    file_info = file_info
  )
}

#' Build a simplified parse tree from parse data.
#'
#' @param parse_data Data frame returned by `getParseData()`.
#' @return Nested list representing the AST in a lightweight form.
#' @keywords internal
build_ast <- function(parse_data) {
  if (is.null(parse_data)) {
    return(list())
  }
  nodes <- split(parse_data, parse_data$id)
  lapply(nodes, function(node) {
    list(
      token = node$text,
      type = node$token[[1]],
      line1 = node$line1[[1]],
      col1 = node$col1[[1]],
      line2 = node$line2[[1]],
      col2 = node$col2[[1]]
    )
  })
}
