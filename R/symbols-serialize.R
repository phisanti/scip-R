# C++ RESULT CONVERTERS / SCIP RANGE SERIALIZATION
# Split from symbols.R: df_to_symbol_list / df_to_occurrence_list / to_scip_range.
# Move-only; no logic change.

#' @include parser.R
NULL

#' Convert C++ symbols DataFrame to list format.
#'
#' Transforms the columnar DataFrame from resolve_symbols_cpp() into a list
#' of symbol records, each containing symbol ID, display name, and kind.
#'
#' @param df DataFrame with columns:
#'   \describe{
#'     \item{symbol}{Character: unique symbol ID}
#'     \item{display_name}{Character: human-readable name}
#'     \item{kind}{Character: "function", "variable", "parameter", "call"}
#'   }
#' @return List of symbol entries, each a list with components:
#'   symbol, display_name, kind, documentation (empty placeholder)
#' @keywords internal
#' @seealso \code{\link{resolve_symbols_cpp}} for the C++ implementation
df_to_symbol_list <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }


  # Note: documentation starts as empty placeholder because C++ doesn't extract

  # roxygen docs. Post-processing in resolve_symbols() populates this field.
  lapply(seq_len(nrow(df)), function(i) {
    list(
      symbol = df$symbol[i],
      display_name = df$display_name[i],
      kind = df$kind[i],
      documentation = list()
    )
  })
}

#' Convert C++ occurrences DataFrame to list format.
#'
#' Transforms the columnar DataFrame from resolve_symbols_cpp() into a list
#' of occurrence records, each containing symbol ID, source range, and role.
#'
#' @param df DataFrame with columns:
#'   \describe{
#'     \item{symbol}{Character: symbol ID this occurrence refers to}
#'     \item{line1, col1}{Integer: start position (1-based)}
#'     \item{line2, col2}{Integer: end position (1-based)}
#'     \item{role}{Character: "definition" or "reference"}
#'   }
#' @return List of occurrence entries, each a list with components:
#'   symbol, range (4-element vector), role
#' @keywords internal
#' @seealso \code{\link{resolve_symbols_cpp}} for the C++ implementation
df_to_occurrence_list <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }

  lapply(seq_len(nrow(df)), function(i) {
    list(
      symbol = df$symbol[i],
      range = to_scip_range(df$line1[i], df$col1[i], df$line2[i], df$col2[i]),
      role = df$role[i]
    )
  })
}

#' Convert a parse-data range (1-based) into SCIP range coordinates (0-based).
#'
#' R parse data uses 1-based line/column coordinates with an inclusive end
#' column. SCIP expects 0-based line/column with an exclusive end column, so
#' line1/col1/line2 are shifted by -1 while col2 is preserved.
#'
#' @param line1 Start line (1-based).
#' @param col1 Start column (1-based).
#' @param line2 End line (1-based).
#' @param col2 End column (1-based inclusive).
#' @return Integer vector `c(start_line, start_col, end_line, end_col)` for SCIP.
#' @keywords internal
to_scip_range <- function(line1, col1, line2, col2) {
  c(
    as.integer(line1) - 1L,
    as.integer(col1) - 1L,
    as.integer(line2) - 1L,
    as.integer(col2)
  )
}
