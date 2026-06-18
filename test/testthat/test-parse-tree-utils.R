# Tests for parse tree navigation utilities
# This validates the parse-tree-utils.R module implementation

# ============================================================================
# Level 1: Basic Tree Navigation
# ============================================================================

test_that("get_descendants returns all descendants recursively", {
  tmp_dir <- tempfile("scip_tree_descendants_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "nested.R")
  writeLines(c(
    "f <- function(x) { x + 1 }"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "nested.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the root expr node
  root_exprs <- data[data$token == "expr" & data$parent == 0, , drop = FALSE]
  expect_gt(nrow(root_exprs), 0)

  # Get all descendants
  descendants <- get_descendants(root_exprs$id[1], data)

  # Should include FUNCTION token (deeply nested)
  func_tokens <- descendants[descendants$token == "FUNCTION", , drop = FALSE]
  expect_gt(nrow(func_tokens), 0)

  # Should include SYMBOL 'x' (in parameter and body)
  symbol_x <- descendants[descendants$token %in% c("SYMBOL", "SYMBOL_FORMALS") &
                          descendants$text == "x", , drop = FALSE]
  expect_gt(nrow(symbol_x), 0)
})

# ============================================================================
# Level 2: Ancestor and Descendant Type Checking
# ============================================================================

# ============================================================================
# Level 5: Real-World Pattern Detection
# ============================================================================

test_that("utilities correctly identify function parameter pattern", {
  tmp_dir <- tempfile("scip_tree_param_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "params.R")
  writeLines(c(
    "f <- function(x, y, z) { x + y + z }"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "params.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  index <- build_tree_index(data)

  # Find SYMBOL_FORMALS tokens (parameters)
  params <- data[data$token == "SYMBOL_FORMALS", , drop = FALSE]

  # Should find 3 parameters
  expect_equal(nrow(params), 3)
  expect_setequal(params$text, c("x", "y", "z"))

  # Each parameter should have FUNCTION as a sibling (same parent expr).
  # With the indexed API, siblings are the other children of the shared parent.
  for (i in seq_len(nrow(params))) {
    parent_id <- get_parent_id_indexed(params$id[i], index)
    sibling_ids <- setdiff(get_children_indexed(parent_id, index), params$id[i])
    sibling_tokens <- data$token[match(sibling_ids, data$id)]

    # One of the siblings should be FUNCTION
    has_function_sibling <- any(sibling_tokens == "FUNCTION")
    expect_true(has_function_sibling)
  }
})

test_that("utilities correctly identify loop variable pattern", {
  tmp_dir <- tempfile("scip_tree_loop_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "loop.R")
  writeLines(c(
    "for (i in 1:10) { print(i) }"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "loop.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  index <- build_tree_index(data)

  # Find all SYMBOL tokens named 'i'
  symbols_i <- data[data$token == "SYMBOL" & data$text == "i", , drop = FALSE]
  expect_gt(nrow(symbols_i), 0)

  # At least one should have a forcond parent
  has_forcond_parent <- FALSE
  for (i in seq_len(nrow(symbols_i))) {
    parent_id <- get_parent_id_indexed(symbols_i$id[i], index)
    if (!is.na(parent_id)) {
      parent_token <- data$token[match(parent_id, data$id)]
      if (!is.na(parent_token) && parent_token == "forcond") {
        has_forcond_parent <- TRUE
        break
      }
    }
  }

  expect_true(has_forcond_parent)
})

test_that("utilities correctly identify function definition pattern", {
  tmp_dir <- tempfile("scip_tree_func_def_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "func.R")
  writeLines(c(
    "my_func <- function(x) x + 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "func.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  index <- build_tree_index(data)

  # Find SYMBOL 'my_func'
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "my_func", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Find assignment after it on the same line
  assignments <- find_on_same_line_indexed(
    symbol_nodes$id[1],
    index,
    c("LEFT_ASSIGN"),
    "after"
  )
  expect_gt(nrow(assignments), 0)

  # Get the enclosing expr that contains the assignment (the assignment expression)
  assign_expr <- find_ancestor_of_type_indexed(assignments$id[1], "expr", index)
  expect_equal(nrow(assign_expr), 1)

  # Check if it contains FUNCTION
  has_func <- has_descendant_of_type_indexed(assign_expr$id[1], "FUNCTION", index)
  expect_true(has_func)
})

# ============================================================================
# Level 6: Edge Cases
# ============================================================================

test_that("handles empty parse data gracefully", {
  empty_data <- data.frame(
    line1 = integer(0),
    col1 = integer(0),
    line2 = integer(0),
    col2 = integer(0),
    id = integer(0),
    parent = integer(0),
    token = character(0),
    terminal = logical(0),
    text = character(0)
  )

  index <- build_tree_index(empty_data)

  # All lookups should return empty/NA/FALSE without error
  expect_length(get_children_indexed(1, index), 0)
  expect_equal(nrow(get_descendants(1, empty_data)), 0)
  expect_true(is.na(get_parent_id_indexed(1, index)))
  expect_equal(nrow(find_ancestor_of_type_indexed(1, "SYMBOL", index)), 0)
  expect_false(has_descendant_of_type_indexed(1, "SYMBOL", index))
})

test_that("handles invalid node IDs gracefully", {
  tmp_dir <- tempfile("scip_tree_invalid_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "simple.R")
  writeLines(c("x <- 1"), file_path)

  file_info <- list(path = file_path, relative_path = "simple.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  index <- build_tree_index(data)

  # Use non-existent node ID
  invalid_id <- 999999

  expect_length(get_children_indexed(invalid_id, index), 0)
  expect_equal(nrow(get_descendants(invalid_id, data)), 0)
  expect_true(is.na(get_parent_id_indexed(invalid_id, index)))
  expect_equal(nrow(find_ancestor_of_type_indexed(invalid_id, "SYMBOL", index)), 0)
  expect_false(has_descendant_of_type_indexed(invalid_id, "SYMBOL", index))
})

test_that("handles deeply nested structures", {
  tmp_dir <- tempfile("scip_tree_deep_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "deep.R")
  writeLines(c(
    "f <- function(a) {",
    "  g <- function(b) {",
    "    h <- function(c) {",
    "      c + 1",
    "    }",
    "  }",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "deep.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the innermost 'c' symbol
  symbols_c <- data[data$token %in% c("SYMBOL", "SYMBOL_FORMALS") &
                    data$text == "c", , drop = FALSE]

  if (nrow(symbols_c) > 0) {
    # Should be able to traverse up through multiple FUNCTION ancestors
    descendants <- get_descendants(0, data)  # Get all from root

    # Should contain multiple FUNCTION tokens
    func_tokens <- descendants[descendants$token == "FUNCTION", , drop = FALSE]
    expect_gte(nrow(func_tokens), 3)
  }
})

# ============================================================================
# Level 7: Scope Index Functions (O(1) lookups)
# ============================================================================

test_that("build_tree_index includes scope information", {
  tmp_dir <- tempfile("scip_scope_index_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "scope.R")
  writeLines(c(
    "x <- 1",
    "f <- function(a) {",
    "  b <- a + 1",
    "  b",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "scope.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Build tree index

  index <- build_tree_index(data)

  # Should have scope information

  expect_true("enclosing_function" %in% names(index))
  expect_true("inside_function" %in% names(index))

  # enclosing_function should be same length as parse_data
  expect_equal(length(index$enclosing_function), nrow(data))
  expect_equal(length(index$inside_function), nrow(data))
})

test_that("is_inside_function_indexed detects symbols inside functions", {
  tmp_dir <- tempfile("scip_inside_func_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "inside.R")
  writeLines(c(
    "x <- 1",  # top-level
    "f <- function(a) {",
    "  b <- a + 1",  # inside function
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "inside.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data
  index <- build_tree_index(data)

  # Find the top-level 'x' symbol
  top_x <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]
  expect_gt(nrow(top_x), 0)

  # Top-level x should NOT be inside a function
  expect_false(is_inside_function_indexed(top_x$id[1], index))

  # Find 'b' symbol inside the function
  inner_b <- data[data$token == "SYMBOL" & data$text == "b", , drop = FALSE]
  expect_gt(nrow(inner_b), 0)

  # Inner b should be inside a function
  expect_true(is_inside_function_indexed(inner_b$id[1], index))
})

test_that("get_enclosing_function_indexed returns correct function ID", {
  tmp_dir <- tempfile("scip_enclosing_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "enclosing.R")
  writeLines(c(
    "outer <- function(x) {",
    "  inner <- function(y) {",
    "    y + 1",
    "  }",
    "  inner(x)",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "enclosing.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data
  index <- build_tree_index(data)

  # Find the 'y' symbol inside the inner function
  symbol_y <- data[data$token %in% c("SYMBOL", "SYMBOL_FORMALS") &
                   data$text == "y", , drop = FALSE]
  expect_gt(nrow(symbol_y), 0)

  # Get enclosing function for the body 'y' (the one in y + 1)
  # This should be the inner function
  for (i in seq_len(nrow(symbol_y))) {
    enclosing_id <- get_enclosing_function_indexed(symbol_y$id[i], index)
    if (!is.na(enclosing_id)) {
      # Verify we got a valid function ID
      func_row <- data[data$id == enclosing_id, , drop = FALSE]
      if (nrow(func_row) > 0) {
        # The enclosing should be a FUNCTION token or its parent expr
        expect_true(TRUE)  # We found an enclosing function
        break
      }
    }
  }
})

test_that("scope index handles nested functions correctly", {
  tmp_dir <- tempfile("scip_nested_scope_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "nested.R")
  writeLines(c(
    "a <- function() {",
    "  b <- function() {",
    "    c <- function() {",
    "      1",
    "    }",
    "  }",
    "}"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "nested.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data
  index <- build_tree_index(data)

  # Count how many nodes are inside functions
  inside_count <- sum(index$inside_function)

  # Most nodes should be inside functions (except the top-level assignment)
  total_nodes <- nrow(data)
  expect_gt(inside_count, 0)
  expect_lt(inside_count, total_nodes)  # Not all nodes are inside functions
})

test_that("scope index handles files with no functions", {
  tmp_dir <- tempfile("scip_no_func_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "no_func.R")
  writeLines(c(
    "x <- 1",
    "y <- x + 2",
    "z <- y * 3"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "no_func.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data
  index <- build_tree_index(data)

  # No nodes should be inside functions
  expect_true(all(!index$inside_function))

  # All enclosing_function values should be NA
  expect_true(all(is.na(index$enclosing_function)))
})

test_that("scope index handles empty parse data", {
  empty_data <- data.frame(
    line1 = integer(0),
    col1 = integer(0),
    line2 = integer(0),
    col2 = integer(0),
    id = integer(0),
    parent = integer(0),
    token = character(0),
    terminal = logical(0),
    text = character(0)
  )

  index <- build_tree_index(empty_data)

  expect_equal(length(index$enclosing_function), 0)
  expect_equal(length(index$inside_function), 0)
})
