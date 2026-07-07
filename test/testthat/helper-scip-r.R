# This helper sources implementation scripts for the scip-r tests.
# It guarantees CLI and indexing modules are loaded into the test environment.
# Future refactors should replace manual sourcing with a package namespace.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat must be installed to run helper setup.")
}

project_root <- normalizePath(file.path(testthat::test_path("."), "..", ".."), winslash = "/", mustWork = TRUE)
r_dir <- file.path(project_root, "R")
r_proto <- normalizePath(
  file.path(project_root, "..", "..", "..", "references", "scip", "scip.proto"),
  winslash = "/",
  mustWork = FALSE
)
if (file.exists(r_proto)) {
  Sys.setenv(SCIP_PROTO_PATH = r_proto)
}
r_scripts <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

for (script_path in r_scripts) {
  sys.source(script_path, envir = globalenv())
}

scipr_read_index <- function(path) {
  find_proto_path(project_root)
  RProtoBuf::read(RProtoBuf::P("scip.Index"), path)
}

scipr_document <- function(index, relative_path) {
  docs <- Filter(
    function(doc) identical(doc$relative_path, relative_path),
    index$documents
  )
  testthat::expect_length(docs, 1)
  docs[[1]]
}

scipr_symbols_by_id <- function(document) {
  stats::setNames(
    as.list(document$symbols),
    vapply(document$symbols, function(symbol) symbol$symbol, character(1))
  )
}

scipr_occurrences_for <- function(document, symbol, role = NULL) {
  matches <- Filter(
    function(occurrence) identical(occurrence$symbol, symbol),
    as.list(document$occurrences)
  )

  if (is.null(role)) {
    return(matches)
  }

  role_enum <- RProtoBuf::P("scip.SymbolRole")
  role_value <- switch(role,
    definition = as.integer(role_enum$Definition),
    reference = as.integer(role_enum$ReadAccess),
    testthat::fail(sprintf("Unknown SCIP occurrence role: %s", role))
  )
  Filter(
    function(occurrence) bitwAnd(as.integer(occurrence$symbol_roles), role_value) != 0L,
    matches
  )
}

scipr_symbol_kind <- function(kind) {
  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  as.integer(kind_enum[[kind]])
}
