# This test verifies CLI parsing behaviour for default and custom inputs.
# It asserts configuration results feed downstream indexing modules correctly.
# Future tests should cover orchestrator execution paths and error handling.

testthat::test_that("CLI parser applies defaults", {
  cfg <- parse_cli_args(character())
  testthat::expect_type(cfg, "list")
  testthat::expect_true(nzchar(cfg$root))
  testthat::expect_identical(cfg$output, "index.scip")
})

testthat::test_that("CLI parser overrides output", {
  cfg <- parse_cli_args(c("--root", ".", "--output", "custom.scip"))
  testthat::expect_identical(cfg$output, "custom.scip")
})
