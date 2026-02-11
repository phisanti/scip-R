# This test confirms occurrence mapping forwards analyzer data unchanged.
# It feeds a fabricated occurrence list and checks the mapper returns it.
# Future iterations should assert encoding and enclosing range behaviour.

testthat::test_that("map_occurrences returns analyzer output", {
  occurrences <- list(list(symbol = "local a#b", range = c(0, 0, 0, 1), role = "definition"))
  mapped <- map_occurrences(list(occurrences = occurrences), NULL)
  testthat::expect_identical(mapped, occurrences)
})

# ============================================================================
# Anti-regression tests for vectorized match()-based node lookup
# BUG FIX: map_occurrences was O(n*m) due to per-occurrence data.frame scans.
# Replaced with vectorized match() for O(n+m) complexity.
# These tests ensure the optimized path produces correct enclosing ranges.
# ============================================================================

testthat::test_that("map_occurrences computes enclosing ranges for definitions", {
  # Parse a simple assignment: x <- 1
  code <- "x <- 1"
  file_info <- list(path = "test.R", relative_path = "test.R")
  parsed <- parse(text = code, keep.source = TRUE)
  parse_data <- getParseData(parsed)

  # Create a definition occurrence for 'x' at its actual position
  x_row <- parse_data[parse_data$text == "x" & parse_data$token == "SYMBOL", ]
  testthat::expect_gt(nrow(x_row), 0)

  occurrences <- list(list(
    symbol = "local test#x",
    range = c(x_row$line1[1] - 1L, x_row$col1[1] - 1L, x_row$line2[1] - 1L, x_row$col2[1]),
    role = "definition"
  ))

  ast_module <- list(parse_data = parse_data, file_info = file_info)
  result <- map_occurrences(list(occurrences = occurrences), ast_module)

  # Definition should get an enclosing range (the assignment expression)
  testthat::expect_true(!is.null(result[[1]]$enclosing_range))
  testthat::expect_length(result[[1]]$enclosing_range, 4)
})

testthat::test_that("map_occurrences computes enclosing ranges for references", {
  # Parse code with a reference: x <- 1; y <- x
  code <- "x <- 1\ny <- x"
  file_info <- list(path = "test.R", relative_path = "test.R")
  parsed <- parse(text = code, keep.source = TRUE)
  parse_data <- getParseData(parsed)

  # Find the reference to 'x' on line 2 (the RHS of y <- x)
  x_rows <- parse_data[parse_data$text == "x" & parse_data$token == "SYMBOL", ]
  # The second 'x' is the reference (line 2)
  x_ref <- x_rows[x_rows$line1 == 2, ]
  testthat::expect_gt(nrow(x_ref), 0)

  occurrences <- list(list(
    symbol = "local test#x",
    range = c(x_ref$line1[1] - 1L, x_ref$col1[1] - 1L, x_ref$line2[1] - 1L, x_ref$col2[1]),
    role = "reference"
  ))

  ast_module <- list(parse_data = parse_data, file_info = file_info)
  result <- map_occurrences(list(occurrences = occurrences), ast_module)

  # Reference should get an enclosing range
  testthat::expect_true(!is.null(result[[1]]$enclosing_range))
  testthat::expect_length(result[[1]]$enclosing_range, 4)
})

testthat::test_that("map_occurrences preserves closure_func_range for list closures", {
  # Closure definitions carry pre-computed ranges and skip tree walking
  occurrences <- list(list(
    symbol = "local test#mod$server",
    range = c(0, 10, 0, 16),
    role = "definition",
    closure_func_range = c(0, 20, 5, 1)
  ))

  result <- map_occurrences(list(occurrences = occurrences), list(parse_data = NULL))

  # closure_func_range should be moved to enclosing_range
  testthat::expect_identical(result[[1]]$enclosing_range, c(0, 20, 5, 1))
  # Internal field should be cleaned up
  testthat::expect_null(result[[1]]$closure_func_range)
})

testthat::test_that("map_occurrences handles mixed closure and regular occurrences", {
  code <- "f <- function(x) x + 1"
  file_info <- list(path = "test.R", relative_path = "test.R")
  parsed <- parse(text = code, keep.source = TRUE)
  parse_data <- getParseData(parsed)

  f_row <- parse_data[parse_data$text == "f" & parse_data$token == "SYMBOL", ]
  testthat::expect_gt(nrow(f_row), 0)

  occurrences <- list(
    # Regular definition
    list(
      symbol = "local test#f",
      range = c(f_row$line1[1] - 1L, f_row$col1[1] - 1L, f_row$line2[1] - 1L, f_row$col2[1]),
      role = "definition"
    ),
    # Closure with pre-computed range
    list(
      symbol = "local test#mod$init",
      range = c(10, 5, 10, 9),
      role = "definition",
      closure_func_range = c(10, 12, 20, 1)
    )
  )

  ast_module <- list(parse_data = parse_data, file_info = file_info)
  result <- map_occurrences(list(occurrences = occurrences), ast_module)

  # First occurrence: regular definition, should have enclosing range from tree walk
  testthat::expect_true(!is.null(result[[1]]$enclosing_range))
  testthat::expect_length(result[[1]]$enclosing_range, 4)

  # Second occurrence: closure, enclosing_range from pre-computed func_range
  testthat::expect_identical(result[[2]]$enclosing_range, c(10, 12, 20, 1))
  testthat::expect_null(result[[2]]$closure_func_range)
})

testthat::test_that("map_occurrences accepts pre-built tree_index", {
  # Verify that passing a tree_index avoids redundant rebuilding
  code <- "x <- 1"
  file_info <- list(path = "test.R", relative_path = "test.R")
  parsed <- parse(text = code, keep.source = TRUE)
  parse_data <- getParseData(parsed)

  tree_index <- build_tree_index(parse_data)

  x_row <- parse_data[parse_data$text == "x" & parse_data$token == "SYMBOL", ]
  occurrences <- list(list(
    symbol = "local test#x",
    range = c(x_row$line1[1] - 1L, x_row$col1[1] - 1L, x_row$line2[1] - 1L, x_row$col2[1]),
    role = "definition"
  ))

  ast_module <- list(parse_data = parse_data, file_info = file_info)

  # Should work identically with pre-built tree_index
  result_with <- map_occurrences(list(occurrences = occurrences), ast_module, tree_index = tree_index)
  result_without <- map_occurrences(list(occurrences = occurrences), ast_module)

  testthat::expect_identical(result_with[[1]]$enclosing_range, result_without[[1]]$enclosing_range)
})

testthat::test_that("tree index uses environment-based O(1) lookups", {
  # Verify the tree index returns environments, not named vectors
  # This prevents regression to O(n) lookups via %in% names() pattern
  code <- "x <- function(a) a + 1"
  parsed <- parse(text = code, keep.source = TRUE)
  parse_data <- getParseData(parsed)

  tree_index <- build_tree_index(parse_data)

  # Core structures should be environments for O(1) lookup
  testthat::expect_true(is.environment(tree_index$children))
  testthat::expect_true(is.environment(tree_index$parent))
  testthat::expect_true(is.environment(tree_index$rows))
  testthat::expect_true(is.environment(tree_index$by_line))
  testthat::expect_true(is.environment(tree_index$has_func_on_line))

  # Accessor functions should still work correctly
  # Find a node with children (the top-level expr)
  top_expr <- parse_data[parse_data$token == "expr" & parse_data$parent == 0, ]
  if (nrow(top_expr) > 0) {
    children <- get_children_indexed(top_expr$id[1], tree_index)
    testthat::expect_true(length(children) > 0)
  }

  # Test parent lookup
  symbol_row <- parse_data[parse_data$text == "x" & parse_data$token == "SYMBOL", ]
  if (nrow(symbol_row) > 0) {
    parent_id <- get_parent_id_indexed(symbol_row$id[1], tree_index)
    testthat::expect_false(is.na(parent_id))
  }

  # Test row lookup for existing and non-existing IDs
  row_idx <- get_row_indexed(parse_data$id[1], tree_index)
  testthat::expect_false(is.na(row_idx))

  missing_row <- get_row_indexed(999999L, tree_index)
  testthat::expect_true(is.na(missing_row))

  # Test missing parent returns NA
  missing_parent <- get_parent_id_indexed(999999L, tree_index)
  testthat::expect_true(is.na(missing_parent))

  # Test missing children returns empty integer
  missing_children <- get_children_indexed(999999L, tree_index)
  testthat::expect_identical(missing_children, integer(0))
})
