# Tests for scope encoding in local symbol IDs.
#
# Scope encoding adds the enclosing function name as a second hash segment
# in local symbol IDs, enabling dead code detection to distinguish:
#   - Top-level file assignments: "local path#name"       (1 hash, dead code candidate)
#   - Function-local variables:   "local path#func#name"  (2 hashes, skip for dead code)
#
# Without scope encoding, dead code detection has a ~90% false positive rate
# because it cannot tell local variables from top-level definitions.
#
# Issue: scip-R-ml5

# ============================================================================
# Helper: count hash separators in a symbol ID
# ============================================================================
count_hashes <- function(symbol_id) {
  nchar(gsub("[^#]", "", symbol_id))
}

# ============================================================================
# Unit Tests: build_function_name_map()
# ============================================================================

test_that("build_function_name_map identifies named functions", {
  code <- '
    my_func <- function(x) { x + 1 }
    another <- function(a, b) { a * b }
    top_var <- 42
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  parsed <- parse(tmp, keep.source = TRUE)
  data <- getParseData(parsed, includeText = TRUE)

  func_names <- build_function_name_map(data)

  # Should have entries for both functions
  all_names <- ls(func_names)
  expect_true(length(all_names) >= 2)

  # The values should be the function names
  name_values <- vapply(all_names, function(k) func_names[[k]], character(1))
  expect_true("my_func" %in% name_values)
  expect_true("another" %in% name_values)
})

test_that("build_function_name_map skips anonymous functions", {
  code <- '
    result <- lapply(1:10, function(x) x * 2)
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  parsed <- parse(tmp, keep.source = TRUE)
  data <- getParseData(parsed, includeText = TRUE)

  func_names <- build_function_name_map(data)
  all_names <- ls(func_names)

  # The anonymous function inside lapply should not get a name entry
  # (there is no assignment on its line for it)
  # result <- lapply(...) is an assignment, but it's not a FUNCTION token line
  name_values <- if (length(all_names) > 0) {
    vapply(all_names, function(k) func_names[[k]], character(1))
  } else {
    character(0)
  }

  # "result" is assigned via lapply, not via function keyword
  # The anonymous function has no name
  expect_false("result" %in% name_values)
})

test_that("build_function_name_map handles dotted function names", {
  code <- '
    pgx.createPGX <- function(counts, samples) { NULL }
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  parsed <- parse(tmp, keep.source = TRUE)
  data <- getParseData(parsed, includeText = TRUE)

  func_names <- build_function_name_map(data)
  all_names <- ls(func_names)
  name_values <- vapply(all_names, function(k) func_names[[k]], character(1))

  expect_true("pgx.createPGX" %in% name_values)
})

# ============================================================================
# Unit Tests: get_enclosing_function_name()
# ============================================================================

test_that("get_enclosing_function_name returns NULL for top-level nodes", {
  code <- '
    top_var <- 42
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  file_info <- list(path = tmp, relative_path = "test.R")
  ast <- parse_file(file_info)
  tree_index <- build_tree_index(ast$parse_data)

  # Find the SYMBOL for top_var
  sym_rows <- ast$parse_data[ast$parse_data$token == "SYMBOL" &
                              ast$parse_data$text == "top_var", , drop = FALSE]
  expect_true(nrow(sym_rows) > 0)

  name <- get_enclosing_function_name(sym_rows$id[1], tree_index)
  expect_null(name)
})

test_that("get_enclosing_function_name returns function name for local nodes", {
  code <- '
    my_func <- function(x) {
      local_var <- x + 1
      local_var
    }
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  file_info <- list(path = tmp, relative_path = "test.R")
  ast <- parse_file(file_info)
  tree_index <- build_tree_index(ast$parse_data)

  # Find the SYMBOL for local_var (inside my_func)
  sym_rows <- ast$parse_data[ast$parse_data$token == "SYMBOL" &
                              ast$parse_data$text == "local_var", , drop = FALSE]
  expect_true(nrow(sym_rows) > 0)

  name <- get_enclosing_function_name(sym_rows$id[1], tree_index)
  expect_equal(name, "my_func")
})

test_that("get_enclosing_function_name returns innermost function for nested", {
  code <- '
    outer_func <- function() {
      inner_func <- function() {
        deeply_nested <- 123
      }
    }
  '
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(code, tmp)

  file_info <- list(path = tmp, relative_path = "test.R")
  ast <- parse_file(file_info)
  tree_index <- build_tree_index(ast$parse_data)

  # Find the SYMBOL for deeply_nested
  sym_rows <- ast$parse_data[ast$parse_data$token == "SYMBOL" &
                              ast$parse_data$text == "deeply_nested", , drop = FALSE]
  expect_true(nrow(sym_rows) > 0)

  name <- get_enclosing_function_name(sym_rows$id[1], tree_index)
  expect_equal(name, "inner_func")
})

# ============================================================================
# Integration Tests: resolve_symbols() with scope encoding
# ============================================================================

test_that("top-level variables get 1-hash symbol IDs", {
  tmp_dir <- tempfile("scip_scope_toplevel_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "top_level_var <- 42",
    "another_top_level <- 'hello'"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Find symbol for top_level_var
  top_sym <- Filter(function(s) s$display_name == "top_level_var", result$symbols)
  expect_equal(length(top_sym), 1)

  # Should have exactly 1 hash separator (local path#name)
  expect_equal(count_hashes(top_sym[[1]]$symbol), 1)
  expect_true(grepl("^local test_R#", top_sym[[1]]$symbol))
})

test_that("local variables inside named functions get 2-hash symbol IDs", {
  tmp_dir <- tempfile("scip_scope_local_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "my_function <- function(x) {",
    "  local_var <- x + 1",
    "  another_local <- x * 2",
    "  local_var + another_local",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Find symbol for local_var
  local_sym <- Filter(function(s) s$display_name == "local_var", result$symbols)
  expect_equal(length(local_sym), 1)

  # Should have exactly 2 hash separators (local path#func#name)
  expect_equal(count_hashes(local_sym[[1]]$symbol), 2)
  expect_true(grepl("my_function#local_var", local_sym[[1]]$symbol))
})

test_that("function parameters inside named functions get 2-hash symbol IDs", {
  tmp_dir <- tempfile("scip_scope_params_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "my_function <- function(param_x, param_y) {",
    "  param_x + param_y",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Find symbol for param_x
  param_sym <- Filter(function(s) s$display_name == "param_x", result$symbols)
  expect_equal(length(param_sym), 1)

  # Parameters inside named functions should get 2-hash IDs
  expect_equal(count_hashes(param_sym[[1]]$symbol), 2)
  expect_true(grepl("my_function#param_x", param_sym[[1]]$symbol))
})

test_that("anonymous function locals get 1-hash IDs (no scope name)", {
  tmp_dir <- tempfile("scip_scope_anon_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "result <- lapply(1:10, function(x) {",
    "  temp <- x * 2",
    "  temp",
    "})"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # temp is inside anonymous function -> enclosing_name is NULL -> 1 hash
  temp_sym <- Filter(function(s) s$display_name == "temp", result$symbols)
  if (length(temp_sym) > 0) {
    expect_equal(count_hashes(temp_sym[[1]]$symbol), 1)
  }
})

test_that("deeply nested functions use innermost enclosing name", {
  tmp_dir <- tempfile("scip_scope_nested_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "outer_func <- function() {",
    "  inner_func <- function() {",
    "    deeply_nested_var <- 123",
    "    deeply_nested_var",
    "  }",
    "  inner_func()",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # deeply_nested_var should use innermost function name (inner_func)
  nested_sym <- Filter(function(s) s$display_name == "deeply_nested_var", result$symbols)
  expect_equal(length(nested_sym), 1)

  # Should reference inner_func, not outer_func
  expect_true(grepl("inner_func#deeply_nested_var", nested_sym[[1]]$symbol))
  expect_equal(count_hashes(nested_sym[[1]]$symbol), 2)
})

test_that("dotted function names get backtick-escaped in symbol IDs", {
  tmp_dir <- tempfile("scip_scope_dotted_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "pgx.wgcnaModule <- function() {",
    "  A1 <- 42",
    "  A1",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "R/pgx_wgcna.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # A1 should be inside pgx.wgcnaModule -> 2-hash ID
  a1_sym <- Filter(function(s) s$display_name == "A1", result$symbols)
  expect_equal(length(a1_sym), 1)

  # Should have 2 hashes and backtick-escaped function name
  expect_equal(count_hashes(a1_sym[[1]]$symbol), 2)
  expect_true(grepl("`pgx.wgcnaModule`", a1_sym[[1]]$symbol))
})

test_that("loop variables inside functions get 2-hash IDs", {
  tmp_dir <- tempfile("scip_scope_loop_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "my_func <- function() {",
    "  for (i in 1:10) {",
    "    print(i)",
    "  }",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Loop variable 'i' inside my_func should get 2-hash ID
  i_sym <- Filter(function(s) s$display_name == "i", result$symbols)
  expect_equal(length(i_sym), 1)
  expect_equal(count_hashes(i_sym[[1]]$symbol), 2)
  expect_true(grepl("my_func#i", i_sym[[1]]$symbol))
})

test_that("same variable name in different functions gets distinct IDs", {
  tmp_dir <- tempfile("scip_scope_distinct_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "test.R")
  writeLines(c(
    "func_a <- function() {",
    "  temp <- 1",
    "  temp",
    "}",
    "func_b <- function() {",
    "  temp <- 2",
    "  temp",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "test.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Both 'temp' definitions should exist as separate symbols
  temp_syms <- Filter(function(s) s$display_name == "temp", result$symbols)
  expect_equal(length(temp_syms), 2)

  # They should have different symbol IDs (scoped by function)
  ids <- vapply(temp_syms, function(s) s$symbol, character(1))
  expect_true(ids[1] != ids[2])

  # One should be scoped under func_a, the other under func_b
  expect_true(any(grepl("func_a#temp", ids)))
  expect_true(any(grepl("func_b#temp", ids)))
})

# ============================================================================
# Integration Tests: real-world patterns from playbase
# ============================================================================

test_that("playbase-style function with dotted name encodes scope correctly", {
  # This is the actual pattern from R/pgx_wgcna.R that caused FPs
  tmp_dir <- tempfile("scip_scope_playbase_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "pgx_wgcna.R")
  writeLines(c(
    "pgx.wgcnaModule <- function(pgx, power = NULL, minModuleSize = 30) {",
    "  A1 <- pgx$X",
    "  S1 <- pgx$samples",
    "  F <- abs(cor(t(A1)))",
    "  A1",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "R/pgx_wgcna.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # A1, S1, F should all be 2-hash IDs (inside pgx.wgcnaModule)
  for (var_name in c("A1", "S1", "F")) {
    sym <- Filter(function(s) s$display_name == var_name, result$symbols)
    expect_equal(length(sym), 1, info = paste("Symbol", var_name, "should exist"))
    expect_equal(
      count_hashes(sym[[1]]$symbol), 2,
      info = paste(var_name, "should have 2 hashes, got:", sym[[1]]$symbol)
    )
  }

  # pgx.wgcnaModule itself should be a top-level function (1-hash or global)
  func_sym <- Filter(
    function(s) s$display_name == "pgx.wgcnaModule",
    result$symbols
  )
  # It should exist as a definition
  expect_true(length(func_sym) > 0)
})
