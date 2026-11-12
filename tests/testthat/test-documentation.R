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

# ============================================================================
# Anti-regression: documentation extraction for OOP and specialized symbols
# BUG FIX: S3, S4, R6, Shiny, and list closure symbol builders all had
# documentation = NULL hardcoded, so roxygen docs were silently dropped for
# every non-plain-function symbol. Only basic function definitions populated
# the Documentation field in the SCIP index.
# ============================================================================

testthat::test_that("S3 generic symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' Format an object for display",
    "#' @param x Object to format",
    "#' @export",
    "format_thing <- function(x, ...) UseMethod('format_thing')"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "s3_generic.R"))
  symbols <- build_s3_annotations(ast)

  generic <- Filter(function(s) s$kind == "s3_generic", symbols)
  expect_gte(length(generic), 1)
  expect_false(is.null(generic[[1]]$documentation))
  expect_true(any(grepl("Format an object", generic[[1]]$documentation)))
})

testthat::test_that("S3 method symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' Format a data.frame for display",
    "#' @param x A data.frame",
    "format_thing.data.frame <- function(x, ...) {",
    "  print(x)",
    "}"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "s3_method.R"))
  symbols <- build_s3_annotations(ast)

  method <- Filter(function(s) s$kind == "s3_method", symbols)
  expect_gte(length(method), 1)
  expect_false(is.null(method[[1]]$documentation))
  expect_true(any(grepl("Format a data.frame", method[[1]]$documentation)))
})

testthat::test_that("S4 class symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' A person with a name and age",
    "#' @slot name Character name",
    "#' @export",
    'setClass("Person", slots = c(name = "character", age = "numeric"))'
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "s4_class.R"))
  symbols <- build_s4_symbol_entries(ast, "s4_class.R", NULL)

  cls <- Filter(function(s) s$kind == "s4_class", symbols)
  expect_gte(length(cls), 1)
  expect_false(is.null(cls[[1]]$documentation))
  expect_true(any(grepl("A person with a name", cls[[1]]$documentation)))
})

testthat::test_that("S4 generic symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' Get the name of an object",
    "#' @param object The object",
    "#' @export",
    'setGeneric("getName", function(object) standardGeneric("getName"))'
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "s4_generic.R"))
  symbols <- build_s4_symbol_entries(ast, "s4_generic.R", NULL)

  gen <- Filter(function(s) s$kind == "s4_generic", symbols)
  expect_gte(length(gen), 1)
  expect_false(is.null(gen[[1]]$documentation))
  expect_true(any(grepl("Get the name", gen[[1]]$documentation)))
})

testthat::test_that("S4 method symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' @export",
    "setGeneric(\"greet\", function(object) standardGeneric(\"greet\"))",
    "",
    "#' Greet a person by name",
    "#' @param object A Person object",
    "setMethod(\"greet\", \"Person\", function(object) {",
    "  paste(\"Hello\", object@name)",
    "})"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "s4_method.R"))
  symbols <- build_s4_symbol_entries(ast, "s4_method.R", NULL)

  method <- Filter(function(s) s$kind == "s4_method", symbols)
  expect_gte(length(method), 1)
  expect_false(is.null(method[[1]]$documentation))
  expect_true(any(grepl("Greet a person", method[[1]]$documentation)))
})

testthat::test_that("R6 class symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "#' An animal with a name and sound",
    "#' @description R6 class representing an animal",
    "#' @export",
    "Animal <- R6::R6Class('Animal',",
    "  public = list(",
    "    name = NULL,",
    "    initialize = function(name) {",
    "      self$name <- name",
    "    }",
    "  )",
    ")"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "r6_class.R"))
  symbols <- build_r6_symbol_entries(ast, "r6_class.R", NULL)$symbols

  cls <- Filter(function(s) s$kind == "r6_class", symbols)
  expect_gte(length(cls), 1)
  expect_false(is.null(cls[[1]]$documentation))
  expect_true(any(grepl("An animal with a name", cls[[1]]$documentation)))
})

testthat::test_that("Shiny reactive symbols include roxygen documentation", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  # reactiveVal at top level gets roxygen directly above it
  writeLines(c(
    "#' The current counter value",
    "#' @details Starts at zero, incremented by button clicks",
    "counter <- reactiveVal(0)"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "shiny_reactive.R"))
  symbols <- build_shiny_symbol_entries(ast, "shiny_reactive.R", NULL)

  reactive <- Filter(function(s) s$kind == "shiny_reactive", symbols)
  expect_gte(length(reactive), 1)
  expect_false(is.null(reactive[[1]]$documentation))
  expect_true(any(grepl("current counter value", reactive[[1]]$documentation)))
})

testthat::test_that("undocumented OOP symbols have NULL documentation, not errors", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(c(
    "# No roxygen here, just a plain comment",
    "my_generic <- function(x, ...) UseMethod('my_generic')",
    "",
    'setClass("Bare", slots = c(x = "numeric"))',
    "",
    "Bare6 <- R6::R6Class('Bare6', public = list(x = NULL))"
  ), tmp)

  ast <- parse_file(list(path = tmp, relative_path = "no_docs.R"))

  s3 <- build_s3_annotations(ast)
  s4 <- build_s4_symbol_entries(ast, "no_docs.R", NULL)
  r6 <- build_r6_symbol_entries(ast, "no_docs.R", NULL)$symbols

  # All should succeed without error and have NULL docs
  for (sym in c(s3, s4, r6)) {
    expect_true(is.null(sym$documentation))
  }
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
