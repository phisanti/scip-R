# This test ensures diagnostics aggregator returns a list structure.
# It currently verifies the stub implementation stays stable for empty input.
# Future tests should integrate lintr outputs and parser warnings.

testthat::test_that("collect_diagnostics returns list", {
  result <- collect_diagnostics(list(path = "file.R"), list(), list())
  testthat::expect_true(is.list(result))
  testthat::expect_length(result, 0)
})
