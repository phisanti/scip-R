# This test exercises file discovery for minimal R projects.
# It creates a temporary file and ensures discover_files queues it.
# Future refinements should cover ignore patterns and caching.

testthat::test_that("discover_files finds R source", {
  tmp_dir <- tempfile("scip_r_files_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "script.R")
  writeLines("x <- 1", file_path)

  workspace <- list(
    metadata = list(project_root = tmp_dir),
    ignore = character()
  )

  plan <- discover_files(workspace, cache_enabled = FALSE)
  testthat::expect_equal(length(plan$files), 1)
  testthat::expect_match(plan$files[[1]]$relative_path, "script.R")
})
