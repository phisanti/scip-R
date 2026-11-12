# This test inspects workspace analysis to ensure metadata wiring works.
# It validates fallback behaviour when DESCRIPTION artifacts are absent.

testthat::test_that("workspace metadata resolves defaults", {
  ws <- analyze_workspace(tempdir())
  testthat::expect_true(is.list(ws$metadata))
  testthat::expect_true(nzchar(ws$metadata$project_root))
})

testthat::test_that("analyze_workspace aborts on a nonexistent root", {
  missing_root <- file.path(tempfile("scipr_missing_"), "project")
  testthat::expect_error(analyze_workspace(missing_root), "is not a directory")
})

testthat::test_that("analyze_workspace aborts when root is a file", {
  file_root <- tempfile("scipr_file_root_", fileext = ".R")
  writeLines("x <- 1", file_root)
  on.exit(unlink(file_root), add = TRUE)
  testthat::expect_error(analyze_workspace(file_root), "is not a directory")
})

testthat::test_that("scip-r CLI aborts on a nonexistent --root and writes no index", {
  output <- tempfile("scipr_missing_root_", fileext = ".scip")
  testthat::expect_error(
    scipr_run_cli(c("--root", file.path(tempfile(), "nope"), "--output", output)),
    "is not a directory"
  )
  testthat::expect_false(file.exists(output))
})
