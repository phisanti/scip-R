# Tests for Base R dynamic dispatch detection (do.call, match.fun)
# These test extract_base_dispatch_edges() and resolve_base_dispatch_target()

testthat::test_that("do.call with string literal creates call edge", {
  code <- '
compute_mean <- function(x) mean(x)

result <- do.call("compute_mean", list(x = 1:10))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  testthat::expect_true(any(grepl("compute_mean", vapply(edges, function(e) e$symbol, character(1)))))
  testthat::expect_equal(edges[[1]]$confidence, 1.0)
  testthat::expect_equal(edges[[1]]$target_name, "compute_mean")
  testthat::expect_equal(edges[[1]]$dispatch_type, "do.call")
  testthat::expect_true(edges[[1]]$is_reference)

  unlink(tmp)
})

testthat::test_that("do.call with symbol creates call edge with lower confidence", {
  code <- '
compute_median <- function(x) median(x)

result <- do.call(compute_median, list(x = 1:10))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  testthat::expect_true(any(grepl("compute_median", vapply(edges, function(e) e$symbol, character(1)))))
  # Symbol argument gets confidence 0.9
  testthat::expect_equal(edges[[1]]$confidence, 0.9)
  testthat::expect_equal(edges[[1]]$target_name, "compute_median")
  testthat::expect_equal(edges[[1]]$dispatch_type, "do.call")

  unlink(tmp)
})

testthat::test_that("match.fun with string creates call edge", {
  code <- '
my_func <- function(x) x * 2

f <- match.fun("my_func")
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  testthat::expect_true(any(grepl("my_func", vapply(edges, function(e) e$symbol, character(1)))))
  testthat::expect_equal(edges[[1]]$confidence, 1.0)
  testthat::expect_equal(edges[[1]]$target_name, "my_func")
  testthat::expect_equal(edges[[1]]$dispatch_type, "match.fun")

  unlink(tmp)
})

testthat::test_that("handles base R functions correctly (resolves to base::)", {
  code <- '
result <- do.call("mean", list(x = 1:10))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  # Should resolve to base package, not local symbol
  testthat::expect_true(grepl("base", edges[[1]]$symbol))
  testthat::expect_true(grepl("mean", edges[[1]]$symbol))
  testthat::expect_equal(edges[[1]]$target_name, "mean")
  testthat::expect_equal(edges[[1]]$confidence, 1.0)

  unlink(tmp)
})

testthat::test_that("match.fun with base R function resolves to base::", {
  code <- '
f <- match.fun("median")
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  testthat::expect_true(grepl("base", edges[[1]]$symbol))
  testthat::expect_true(grepl("median", edges[[1]]$symbol))

  unlink(tmp)
})

testthat::test_that("nested dispatch do.call + match.fun extracts target", {
  code <- '
my_func <- function(x) x + 1

result <- do.call(match.fun("my_func"), list(x = 5))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  # Should detect my_func reference (from the string inside match.fun)
  # Both do.call and match.fun will be detected as dispatch calls
  has_my_func <- any(vapply(edges, function(e) grepl("my_func", e$symbol), logical(1)))
  testthat::expect_true(has_my_func)

  unlink(tmp)
})

testthat::test_that("returns empty for non-dispatch code", {
  code <- '
x <- 1 + 2
y <- mean(x)
z <- paste0("hello", "world")
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_equal(length(edges), 0)

  unlink(tmp)
})

testthat::test_that("resolve_base_dispatch_target uses definitions_env first", {
  # Set up a mock definitions environment
  defs_env <- new.env(hash = TRUE, parent = emptyenv())
  defs_env[["my_custom_func"]] <- list(
    symbol_id = "scip-r r mypackage 1.0.0 my_custom_func()."
  )

  # Should find it in definitions_env
  result <- resolve_base_dispatch_target("my_custom_func", defs_env, "test.R")
  testthat::expect_equal(result, "scip-r r mypackage 1.0.0 my_custom_func().")

  # Should fall back to base R for known base functions
  result2 <- resolve_base_dispatch_target("sum", defs_env, "test.R")
  testthat::expect_true(grepl("base", result2))
  testthat::expect_true(grepl("sum", result2))

  # Should fall back to local for unknown functions
  result3 <- resolve_base_dispatch_target("unknown_func", defs_env, "test.R")
  testthat::expect_true(grepl("local", result3))
  testthat::expect_true(grepl("unknown_func", result3))
})

testthat::test_that("resolve_base_dispatch_target returns NULL for empty name", {
  result <- resolve_base_dispatch_target("", NULL, "test.R")
  testthat::expect_null(result)

  result2 <- resolve_base_dispatch_target(NULL, NULL, "test.R")
  testthat::expect_null(result2)
})

testthat::test_that("multiple dispatch calls in same file all detected", {
  code <- '
func_a <- function(x) x + 1
func_b <- function(x) x * 2

result1 <- do.call("func_a", list(x = 1))
result2 <- do.call("func_b", list(x = 2))
result3 <- match.fun("func_a")
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_base_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  # Should find all three dispatch calls
  testthat::expect_equal(length(edges), 3)

  target_names <- vapply(edges, function(e) e$target_name, character(1))
  testthat::expect_true("func_a" %in% target_names)
  testthat::expect_true("func_b" %in% target_names)

  dispatch_types <- vapply(edges, function(e) e$dispatch_type, character(1))
  testthat::expect_equal(sum(dispatch_types == "do.call"), 2)
  testthat::expect_equal(sum(dispatch_types == "match.fun"), 1)

  unlink(tmp)
})
