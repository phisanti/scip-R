# Test roxygen2 documentation extraction

testthat::test_that("extract_roxygen_documentation extracts contiguous comments", {
  tmp_dir <- tempfile("scip_docs_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "documented.R")
  writeLines(c(
    "#' Calculate the sum of two numbers",
    "#' @param x First number",
    "#' @param y Second number",
    "#' @return Sum of x and y",
    "add <- function(x, y) {",
    "  x + y",
    "}"
  ), file_path)

  exprs <- parse(file_path, keep.source = TRUE)
  parse_data <- utils::getParseData(exprs)

  docs <- extract_roxygen_documentation(parse_data, 5)

  expect_false(is.null(docs))
  expect_equal(length(docs), 4)
  expect_true(grepl("Calculate", docs[1]))
  expect_true(grepl("@param x", docs[2]))
  expect_true(grepl("@param y", docs[3]))
  expect_true(grepl("@return", docs[4]))
})

testthat::test_that("extract_roxygen_documentation returns NULL when no docs", {
  tmp_dir <- tempfile("scip_nodocs_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "nodocs.R")
  writeLines(c(
    "# Regular comment, not roxygen",
    "add <- function(x, y) {",
    "  x + y",
    "}"
  ), file_path)

  exprs <- parse(file_path, keep.source = TRUE)
  parse_data <- utils::getParseData(exprs)

  docs <- extract_roxygen_documentation(parse_data, 2)

  expect_null(docs)
})

testthat::test_that("extract_roxygen_documentation stops at non-roxygen line", {
  tmp_dir <- tempfile("scip_gap_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "gap.R")
  writeLines(c(
    "#' Orphan documentation",
    "# Regular comment breaks the chain",
    "#' This should be captured",
    "#' @param x Input",
    "my_func <- function(x) x"
  ), file_path)

  exprs <- parse(file_path, keep.source = TRUE)
  parse_data <- utils::getParseData(exprs)

  docs <- extract_roxygen_documentation(parse_data, 5)

  # Should only get lines 3-4, not line 1 (gap at line 2)
  expect_false(is.null(docs))
  expect_equal(length(docs), 2)
  expect_true(grepl("This should be captured", docs[1]))
  expect_true(grepl("@param x", docs[2]))
})

testthat::test_that("documentation is included in resolved symbols", {
  tmp_dir <- tempfile("scip_full_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "full.R")
  writeLines(c(
    "#' My documented function",
    "#' @param x Input value",
    "my_func <- function(x) x"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "full.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(ast, workspace)

  # Find the function symbol
  func_sym <- Filter(function(s) grepl("my_func", s$symbol) && s$kind == "function", result$symbols)

  expect_gte(length(func_sym), 1)
  expect_false(is.null(func_sym[[1]]$documentation))
  expect_true(any(grepl("My documented function", func_sym[[1]]$documentation)))
})
