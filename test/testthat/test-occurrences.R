# This test confirms occurrence mapping forwards analyzer data unchanged.
# It feeds a fabricated occurrence list and checks the mapper returns it.
# Future iterations should assert encoding and enclosing range behaviour.

testthat::test_that("map_occurrences returns analyzer output", {
  occurrences <- list(list(symbol = "local a#b", range = c(0, 0, 0, 1), role = "definition"))
  mapped <- map_occurrences(list(occurrences = occurrences), NULL)
  testthat::expect_identical(mapped, occurrences)
})
