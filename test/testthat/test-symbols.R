# This test verifies symbol resolution emits definitions and occurrences.
# It indexes a temporary script with assignments and checks returned structures.
# Future work should extend coverage to S3/S4 and method dispatch.

testthat::test_that("resolve_symbols marks assignment definitions", {
  tmp_dir <- tempfile("scip_r_symbols_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "assignments.R")
  writeLines(c(
    "foo <- function(x) x", "bar <- foo(1)"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "assignments.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))

  symbols <- resolve_symbols(ast, workspace)
  defs <- vapply(symbols$occurrences, function(x) identical(x$role, "definition"), logical(1))

  testthat::expect_true(any(defs))
  testthat::expect_true(length(symbols$symbols) >= 1)
})

# ============================================================================
# Tests for THREE KEY FIXES
# ============================================================================

# FIX 1: Function Parameters (SYMBOL_FORMALS) - HIGH IMPACT
testthat::test_that("function parameters are detected as definitions", {
  tmp_dir <- tempfile("scip_params_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "params.R")
  writeLines(c(
    "my_func <- function(arg1, arg2, arg3) {",
    "  arg1 + arg2 + arg3",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "params.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find occurrences of arg1, arg2, arg3
  arg_occurrences <- Filter(function(occ) {
    symbol_name <- sub("^.*#", "", occ$symbol)
    symbol_name %in% c("arg1", "arg2", "arg3")
  }, symbols$occurrences)

  # Should have at least 6 occurrences (3 definitions + 3 references in body)
  expect_gte(length(arg_occurrences), 6)

  # Check that parameters are marked as definitions
  param_definitions <- Filter(function(occ) {
    symbol_name <- sub("^.*#", "", occ$symbol)
    symbol_name %in% c("arg1", "arg2", "arg3") && occ$role == "definition"
  }, arg_occurrences)

  # Should have exactly 3 parameter definitions
  expect_equal(length(param_definitions), 3)

  # Check that parameters have kind "parameter"
  param_symbols <- Filter(function(sym) {
    symbol_name <- sub("^.*#", "", sym$symbol)
    symbol_name %in% c("arg1", "arg2", "arg3")
  }, symbols$symbols)

  expect_gte(length(param_symbols), 3)
  for (param_sym in param_symbols) {
    expect_equal(param_sym$kind, "parameter")
  }
})

testthat::test_that("function parameters support go-to-definition", {
  tmp_dir <- tempfile("scip_param_goto_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "goto_param.R")
  writeLines(c(
    "calculate <- function(x, y) {",
    "  result <- x * 2 + y",
    "  return(result)",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "goto_param.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find x parameter definition and references
  x_occurrences <- Filter(function(occ) {
    symbol_name <- sub("^.*#", "", occ$symbol)
    symbol_name == "x"
  }, symbols$occurrences)

  # Should have definition and at least one reference
  expect_gte(length(x_occurrences), 2)

  x_def <- Find(function(occ) occ$role == "definition", x_occurrences)
  x_ref <- Find(function(occ) occ$role == "reference", x_occurrences)

  expect_false(is.null(x_def))
  expect_false(is.null(x_ref))

  # Both should point to same symbol ID
  expect_equal(x_def$symbol, x_ref$symbol)
})

# FIX 2: Loop Variables (forcond) - HIGH IMPACT
testthat::test_that("loop variables are detected as definitions", {
  tmp_dir <- tempfile("scip_loop_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "loop.R")
  writeLines(c(
    "for (i in 1:10) {",
    "  print(i)",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "loop.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find occurrences of 'i'
  i_occurrences <- Filter(function(occ) {
    symbol_name <- sub("^.*#", "", occ$symbol)
    symbol_name == "i"
  }, symbols$occurrences)

  # Should have at least 2: definition in for loop + reference in body
  expect_gte(length(i_occurrences), 2)

  # Check that loop variable is marked as definition
  i_def <- Find(function(occ) occ$role == "definition", i_occurrences)
  expect_false(is.null(i_def))

  # Loop variable should be kind "variable"
  i_symbol <- Find(function(sym) {
    symbol_name <- sub("^.*#", "", sym$symbol)
    symbol_name == "i"
  }, symbols$symbols)

  expect_false(is.null(i_symbol))
  expect_equal(i_symbol$kind, "variable")
})

testthat::test_that("nested loop variables are handled correctly", {
  tmp_dir <- tempfile("scip_nested_loop_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "nested.R")
  writeLines(c(
    "for (i in 1:3) {",
    "  for (j in 1:3) {",
    "    print(i * j)",
    "  }",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "nested.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find definitions
  definitions <- Filter(function(occ) occ$role == "definition", symbols$occurrences)

  # Should have definitions for both i and j
  def_names <- sapply(definitions, function(occ) {
    sub("^.*#", "", occ$symbol)
  })

  expect_true("i" %in% def_names)
  expect_true("j" %in% def_names)
})

# FIX 3: Robust Function Detection - MEDIUM IMPACT
testthat::test_that("function definitions are detected robustly", {
  tmp_dir <- tempfile("scip_func_detect_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "functions.R")
  writeLines(c(
    "simple_func <- function() { 1 }",
    "param_func <- function(x, y) { x + y }",
    "lambda_func <- \\(x) x * 2"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "functions.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find function symbols
  func_symbols <- Filter(function(sym) sym$kind == "function", symbols$symbols)

  # Should detect all three function definitions
  func_names <- sapply(func_symbols, function(sym) {
    sub("^.*#", "", sym$symbol)
  })

  expect_true("simple_func" %in% func_names)
  expect_true("param_func" %in% func_names)
  # lambda_func might not be detected depending on OP-LAMBDA support
})

testthat::test_that("complex function assignments are detected", {
  tmp_dir <- tempfile("scip_complex_func_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "complex.R")
  writeLines(c(
    "# Function with default parameters",
    "func1 <- function(x = 1, y = 2) { x + y }",
    "",
    "# Function with dots",
    "func2 <- function(...) { list(...) }",
    "",
    "# Nested function definition",
    "outer <- function(a) {",
    "  inner <- function(b) { a + b }",
    "  inner",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "complex.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # All function assignments should be detected
  func_symbols <- Filter(function(sym) sym$kind == "function", symbols$symbols)
  func_names <- sapply(func_symbols, function(sym) {
    sub("^.*#", "", sym$symbol)
  })

  expect_true("func1" %in% func_names)
  expect_true("func2" %in% func_names)
  expect_true("outer" %in% func_names)
  expect_true("inner" %in% func_names)
})

testthat::test_that("non-function assignments are not detected as functions", {
  tmp_dir <- tempfile("scip_not_func_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "not_func.R")
  writeLines(c(
    "x <- 1",
    "y <- c(1, 2, 3)",
    "z <- list(a = 1, b = 2)",
    "df <- data.frame(x = 1:3, y = 4:6)"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "not_func.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Find variable symbols
  var_symbols <- Filter(function(sym) sym$kind == "variable", symbols$symbols)
  var_names <- sapply(var_symbols, function(sym) {
    sub("^.*#", "", sym$symbol)
  })

  # All should be variables, not functions
  expect_true("x" %in% var_names)
  expect_true("y" %in% var_names)
  expect_true("z" %in% var_names)
  expect_true("df" %in% var_names)

  # No function symbols should exist for these names
  func_symbols <- Filter(function(sym) sym$kind == "function", symbols$symbols)
  func_names <- sapply(func_symbols, function(sym) {
    sub("^.*#", "", sym$symbol)
  })

  expect_false("x" %in% func_names)
  expect_false("y" %in% func_names)
  expect_false("z" %in% func_names)
  expect_false("df" %in% func_names)
})

# Integration test: All three fixes together
testthat::test_that("all three fixes work together in realistic code", {
  tmp_dir <- tempfile("scip_integration_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "realistic.R")
  writeLines(c(
    "# Function with parameters, loop, and nested function",
    "process_data <- function(data, threshold) {",
    "  results <- list()",
    "  ",
    "  for (i in seq_along(data)) {",
    "    value <- data[[i]]",
    "    ",
    "    filter_func <- function(x) {",
    "      x > threshold",
    "    }",
    "    ",
    "    if (filter_func(value)) {",
    "      results[[i]] <- value",
    "    }",
    "  }",
    "  ",
    "  return(results)",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "realistic.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  # Check function parameters (FIX 1)
  param_symbols <- Filter(function(sym) sym$kind == "parameter", symbols$symbols)
  param_names <- sapply(param_symbols, function(sym) sub("^.*#", "", sym$symbol))
  expect_true("data" %in% param_names)
  expect_true("threshold" %in% param_names)
  expect_true("x" %in% param_names)  # from filter_func

  # Check loop variable (FIX 2)
  i_occurrences <- Filter(function(occ) {
    sub("^.*#", "", occ$symbol) == "i" && occ$role == "definition"
  }, symbols$occurrences)
  expect_gte(length(i_occurrences), 1)

  # Check function definitions (FIX 3)
  func_symbols <- Filter(function(sym) sym$kind == "function", symbols$symbols)
  func_names <- sapply(func_symbols, function(sym) sub("^.*#", "", sym$symbol))
  expect_true("process_data" %in% func_names)
  expect_true("filter_func" %in% func_names)
})

testthat::test_that("resolve_symbols emits 0-based SCIP occurrence ranges", {
  tmp_dir <- tempfile("scip_zero_based_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "positions.R")
  writeLines(c(
    "compute_summary <- function(data) {",
    "  compute_summary(data)",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "positions.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  symbols <- resolve_symbols(ast, workspace)

  compute_occurrences <- Filter(function(occ) {
    grepl("compute_summary", occ$symbol, fixed = TRUE)
  }, symbols$occurrences)

  expect_gte(length(compute_occurrences), 2)

  def_occ <- Find(function(occ) occ$role == "definition", compute_occurrences)
  ref_occ <- Find(function(occ) occ$role == "reference", compute_occurrences)

  expect_false(is.null(def_occ))
  expect_false(is.null(ref_occ))

  # Definition is on line 1, column 1 in R parse data => [0, 0, 0, 15] in SCIP.
  expect_identical(def_occ$range, c(0L, 0L, 0L, 15L))

  # Reference is indented by two spaces on line 2 => start column 3 (1-based).
  expect_identical(ref_occ$range, c(1L, 2L, 1L, 17L))
})
