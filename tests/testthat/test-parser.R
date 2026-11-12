# This test covers basic parsing to ensure AST data is returned.
# It builds a synthetic script, parses it, and inspects parse_data columns.

testthat::test_that("parse_file returns expressions and parse data", {
  tmp_dir <- tempfile("scip_r_parser_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "sample.R")
  writeLines("add <- function(x, y) x + y", file_path)

  file_info <- list(path = file_path, relative_path = "sample.R")
  ast <- parse_file(file_info)

  testthat::expect_true(length(ast$expressions) >= 1)
  testthat::expect_true(is.data.frame(ast$parse_data))
  testthat::expect_true("token" %in% names(ast$parse_data))
})
