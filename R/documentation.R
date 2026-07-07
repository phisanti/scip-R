# Roxygen documentation extraction for parsed source files.
# Pulls doc comment blocks before symbol definitions for SCIP serialization.
# Inventory: `extract_roxygen_documentation()`, `extract_docs_for_range()`.

#' @include parser.R
NULL

#' Extract roxygen2 documentation for a symbol.
#'
#' Searches for roxygen2 comment blocks (#') immediately preceding
#' the line where a symbol is defined. Returns the raw documentation
#' text with the #' prefix stripped.
#'
#' @param parse_data Data frame from getParseData() containing token information.
#' @param symbol_line Integer line number where the symbol is defined.
#' @return Character vector of documentation lines, or NULL if none found.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' docs <- extract_roxygen_documentation(ast$parse_data, 10)
#' }
extract_roxygen_documentation <- function(parse_data, symbol_line) {
  if (is.null(parse_data) || nrow(parse_data) == 0) {
    return(NULL)
  }

  # Find all COMMENT tokens in the parse data
  comment_rows <- parse_data[parse_data$token == "COMMENT", , drop = FALSE]

  if (nrow(comment_rows) == 0) {
    return(NULL)
  }

  # Filter to roxygen2 comments (start with #')
  roxygen_rows <- comment_rows[grepl("^#'", comment_rows$text), , drop = FALSE]

  if (nrow(roxygen_rows) == 0) {
    return(NULL)
  }

  # Look for contiguous roxygen comments ending just before symbol_line
  doc_lines <- character()
  current_line <- symbol_line - 1

  while (current_line > 0) {
    line_comment <- roxygen_rows[roxygen_rows$line1 == current_line, , drop = FALSE]

    if (nrow(line_comment) == 0) {
      break
    }

    comment_text <- line_comment$text[1]
    stripped <- sub("^#'\\s?", "", comment_text)
    doc_lines <- c(stripped, doc_lines)

    current_line <- current_line - 1
  }

  if (length(doc_lines) == 0) {
    return(NULL)
  }

  doc_lines
}

#' Extract roxygen2 documentation for a symbol at a specific position.
#'
#' @param parse_data Data frame from getParseData().
#' @param range Numeric vector with at least one element (line number).
#' @return Character vector of documentation lines, or NULL if none found.
#' @keywords internal
extract_docs_for_range <- function(parse_data, range) {
  if (is.null(range) || length(range) == 0) {
    return(NULL)
  }
  extract_roxygen_documentation(parse_data, range[1])
}
