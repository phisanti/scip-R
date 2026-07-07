# Qualified symbol extraction for namespace-aware navigation.
# Resolves `pkg::symbol` and `pkg:::symbol` forms and collects external
# dependencies.
# Inventory: `look_backward()`, `extract_qualified_symbols()`,
# `collect_external_symbols()`.

#' @include parser.R
#' @include parse-tree-utils.R
#' @include parse-lookup.R
NULL

#' Parse text backward to extract package-qualified symbol information.
#'
#' This function implements the same regex pattern as languageserver's look_backward()
#' to detect namespace-qualified symbols like `dplyr::mutate` or `pkg:::internal`.
#'
#' @param text Character string to parse (e.g., "dplyr::mutate").
#' @return List with components: full_token, package, accessor, token.
#' @keywords internal
#' @examples
#' \dontrun{
#' look_backward("dplyr::mutate")
#' # Returns: list(full_token="dplyr::mutate", package="dplyr", accessor="::", token="mutate")
#' }
look_backward <- function(text) {
  # Helper to convert NA to empty string
  na_to_empty_string <- function(x) if (is.na(x)) "" else x

  # Regex pattern from languageserver/R/utils.R:443
  # Matches: [package](:: or :::)[symbol]
  # (?<!\\$) - negative lookbehind: not preceded by $
  # (?:\\b|(?=\\.)) - word boundary or followed by dot
  # ([a-zA-Z][a-zA-Z0-9.]+) - package name (must start with letter)
  # (:::?) - accessor: :: or :::
  # ((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)? - symbol name (letters, numbers, dots, underscores)
  pattern <- "(?<!\\$)(?:\\b|(?=\\.))(?:([a-zA-Z][a-zA-Z0-9.]+)(:::?))?((?:[^\\W_]|\\.)(?:[^\\W]|\\.)*)?$"

  matches <- stringi::stri_match_first_regex(text, pattern)

  list(
    full_token = na_to_empty_string(matches[1]),
    package = na_to_empty_string(matches[2]),
    accessor = na_to_empty_string(matches[3]),
    token = na_to_empty_string(matches[4])
  )
}

#' Extract all package-qualified symbols from an AST module.
#'
#' Scans parse data for NS_GET and NS_GET_INT tokens (:: and :::) and extracts
#' the package name and symbol being accessed.
#'
#' @param ast_module List produced by `parse_file()` containing parse data.
#' @return List of qualified symbol information with package, accessor, token, and range.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' extract_qualified_symbols(ast)
#' }
extract_qualified_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # Find all namespace accessor tokens (:: and :::)
  # In R's parse tree, these are tokenized as NS_GET and NS_GET_INT
  ns_rows <- data[data$token %in% c("NS_GET", "NS_GET_INT"), , drop = FALSE]

  if (nrow(ns_rows) == 0) {
    return(list())
  }

  qualified_symbols <- list()

  # Build a lightweight id/parent lookup once per file so the per-NS-row
  # `data[data$id == X, ]` / `data[data$parent == X, ]` scans below become O(1)
  # lookups (row order preserved -> identical output).
  lookup <- build_lookup_index(data)

  for (idx in seq_len(nrow(ns_rows))) {
    ns_row <- ns_rows[idx, ]

    # Get the parent expression that contains this namespace call
    parent_id <- ns_row$parent
    parent_row <- rows_by_id_lookup(parent_id, lookup)

    if (nrow(parent_row) == 0) next

    # Get all children of this namespace expression
    # Structure: expr with children [SYMBOL_PACKAGE, NS_GET, SYMBOL_FUNCTION_CALL/SYMBOL]
    children <- rows_by_parent_lookup(parent_id, lookup)
    children <- children[order(children$col1), , drop = FALSE]

    # Look for SYMBOL_PACKAGE token (the package name)
    package_rows <- children[children$token == "SYMBOL_PACKAGE", , drop = FALSE]
    if (nrow(package_rows) < 1) {
      # Fallback: try SYMBOL if SYMBOL_PACKAGE not found
      package_rows <- children[children$token == "SYMBOL" & children$col1 < ns_row$col1, , drop = FALSE]
      if (nrow(package_rows) < 1) next
    }

    package_name <- package_rows$text[1]

    # After NS_GET token, look for SYMBOL or SYMBOL_FUNCTION_CALL
    symbol_after_ns <- children[
      children$col1 > ns_row$col2 &
        children$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), ,
      drop = FALSE
    ]

    if (nrow(symbol_after_ns) == 0) next

    # Take the first symbol after the namespace operator
    symbol_row <- symbol_after_ns[1, ]
    symbol_name <- symbol_row$text

    # Determine accessor type
    accessor <- if (ns_row$token == "NS_GET") "::" else ":::"

    qualified_symbols[[length(qualified_symbols) + 1]] <- list(
      package = package_name,
      accessor = accessor,
      token = symbol_name,
      token_type = symbol_row$token,
      range = c(symbol_row$line1, symbol_row$col1, symbol_row$line2, symbol_row$col2),
      full_range = c(
        parent_row$line1[1], parent_row$col1[1],
        parent_row$line2[1], parent_row$col2[1]
      )
    )
  }

  qualified_symbols
}

#' Collect external symbols from parsed files and workspace dependencies.
#'
#' Creates placeholder external symbol entries for each package listed in
#' workspace dependencies. This provides a starting point for cross-package
#' symbol resolution.
#'
#' @param parsed_files List of parsed AST modules (currently unused, for future expansion).
#' @param workspace Workspace context with dependency information.
#' @return List of external symbol descriptors grouped by package.
#' @keywords internal
#' @examples
#' \dontrun{
#' workspace <- analyze_workspace(tempdir())
#' externals <- collect_external_symbols(list(), workspace)
#' }
collect_external_symbols <- function(parsed_files, workspace) {
  deps <- workspace$dependencies
  if (is.null(deps) || length(deps) == 0) {
    return(list())
  }

  externals <- list()
  pkg_names <- names(deps)

  for (pkg_name in pkg_names) {
    version <- deps[[pkg_name]]
    if (is.na(version)) version <- "0.0.0"

    externals[[length(externals) + 1]] <- list(
      package = pkg_name,
      version = version,
      symbols = list()
    )
  }

  externals
}
