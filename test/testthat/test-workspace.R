# This test inspects workspace analysis to ensure metadata wiring works.
# It validates fallback behaviour when DESCRIPTION artifacts are absent.
# Future scenarios should simulate lockfiles and ignore pattern handling.

testthat::test_that("workspace metadata resolves defaults", {
  ws <- analyze_workspace(tempdir())
  testthat::expect_true(is.list(ws$metadata))
  testthat::expect_true(nzchar(ws$metadata$project_root))
})
