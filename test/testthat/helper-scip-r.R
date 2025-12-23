# This helper sources implementation scripts for the scip-r tests.
# It guarantees CLI and indexing modules are loaded into the test environment.
# Future refactors should replace manual sourcing with a package namespace.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat must be installed to run helper setup.")
}

project_root <- normalizePath(file.path(testthat::test_path("."), "..", ".."), winslash = "/", mustWork = TRUE)
r_dir <- file.path(project_root, "R")
r_proto <- normalizePath(file.path(project_root, "..", "..", "..", "references", "scip", "scip.proto"), winslash = "/", mustWork = FALSE)
if (file.exists(r_proto)) {
  Sys.setenv(SCIP_PROTO_PATH = r_proto)
}
r_scripts <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

for (script_path in r_scripts) {
  sys.source(script_path, envir = globalenv())
}
