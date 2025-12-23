# This test exercises external symbol collection scaffolding.
# It passes a minimal dependency mapping and expects placeholder output.
# Future enhancements should inspect :: calls and namespace usage.

testthat::test_that("collect_external_symbols mirrors dependency names", {
  workspace <- list(dependencies = c(pkgA = "1.0.0", pkgB = NA))
  externals <- collect_external_symbols(list(), workspace)
  testthat::expect_length(externals, 2)
  testthat::expect_equal(externals[[1]]$package, "pkgA")
})
