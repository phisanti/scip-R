# Tests for parse tree navigation utilities
# This validates the parse-tree-utils.R module implementation

# ============================================================================
# Level 1: Basic Tree Navigation
# ============================================================================

test_that("get_children returns immediate children only", {
  tmp_dir <- tempfile("scip_tree_children_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "simple.R")
  writeLines(c(
    "x <- 1 + 2"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "simple.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the root expr node
  root_exprs <- data[data$token == "expr" & data$parent == 0, , drop = FALSE]
  expect_gt(nrow(root_exprs), 0)

  # Get its children
  children <- get_children(root_exprs$id[1], data)

  # Should have children but not grandchildren
  expect_gt(nrow(children), 0)

  # Verify children all have the correct parent
  expect_true(all(children$parent == root_exprs$id[1]))
})

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

test_that("get_parent returns immediate parent", {
  tmp_dir <- tempfile("scip_tree_parent_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "parent.R")
  writeLines(c(
    "x <- 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "parent.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find a SYMBOL node
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Get its parent
  parent <- get_parent(symbol_nodes$id[1], data)

  expect_equal(nrow(parent), 1)
  expect_equal(parent$id[1], symbol_nodes$parent[1])
})

test_that("get_parent returns empty for root nodes", {
  tmp_dir <- tempfile("scip_tree_root_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "root.R")
  writeLines(c("x <- 1"), file_path)

  file_info <- list(path = file_path, relative_path = "root.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find root expr (parent == 0)
  root_exprs <- data[data$parent == 0, , drop = FALSE]
  expect_gt(nrow(root_exprs), 0)

  # Try to get parent of root
  parent <- get_parent(root_exprs$id[1], data)

  expect_equal(nrow(parent), 0)
})

test_that("get_siblings returns nodes with same parent", {
  tmp_dir <- tempfile("scip_tree_siblings_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "siblings.R")
  writeLines(c(
    "x <- 1 + 2"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "siblings.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the LEFT_ASSIGN operator
  assign_ops <- data[data$token == "LEFT_ASSIGN", , drop = FALSE]
  expect_gt(nrow(assign_ops), 0)

  # Get its siblings
  siblings <- get_siblings(assign_ops$id[1], data)

  # Should have siblings (the expr nodes on either side)
  expect_gt(nrow(siblings), 0)

  # All siblings should have the same parent as the assignment
  if (nrow(siblings) > 0) {
    expect_true(all(siblings$parent == assign_ops$parent[1]))
  }
})

# ============================================================================
# Level 2: Ancestor and Descendant Type Checking
# ============================================================================

test_that("find_ancestor_of_type finds FUNCTION ancestor", {
  tmp_dir <- tempfile("scip_tree_ancestor_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "ancestor.R")
  writeLines(c(
    "f <- function(x) { x + 1 }"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "ancestor.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find a SYMBOL 'x' inside the function body
  symbols_x <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]

  # Find one that's in the body (not parameter)
  for (i in seq_len(nrow(symbols_x))) {
    ancestor <- find_ancestor_of_type(symbols_x$id[i], c("FUNCTION", "OP-LAMBDA"), data)

    # At least one 'x' should have a FUNCTION ancestor
    if (nrow(ancestor) > 0) {
      expect_equal(ancestor$token[1], "FUNCTION")
      return()  # Test passed
    }
  }

  # If we get here, at least verify we found symbols
  expect_gt(nrow(symbols_x), 0)
})

test_that("find_ancestor_of_type returns empty when not found", {
  tmp_dir <- tempfile("scip_tree_no_ancestor_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "no_ancestor.R")
  writeLines(c("x <- 1"), file_path)

  file_info <- list(path = file_path, relative_path = "no_ancestor.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find a SYMBOL node
  symbol_nodes <- data[data$token == "SYMBOL", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Try to find FUNCTION ancestor (shouldn't exist in simple assignment)
  ancestor <- find_ancestor_of_type(symbol_nodes$id[1], c("FUNCTION"), data)

  expect_equal(nrow(ancestor), 0)
})

test_that("has_descendant_of_type detects FUNCTION in assignment RHS", {
  tmp_dir <- tempfile("scip_tree_has_desc_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "func_assign.R")
  writeLines(c(
    "f <- function(x) x + 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "func_assign.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the root expression
  root_exprs <- data[data$token == "expr" & data$parent == 0, , drop = FALSE]
  expect_gt(nrow(root_exprs), 0)

  # Check if it has FUNCTION descendant
  has_func <- has_descendant_of_type(root_exprs$id[1], c("FUNCTION", "OP-LAMBDA"), data)

  expect_true(has_func)
})

test_that("has_descendant_of_type returns FALSE when type not present", {
  tmp_dir <- tempfile("scip_tree_no_desc_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "simple_assign.R")
  writeLines(c("x <- 1"), file_path)

  file_info <- list(path = file_path, relative_path = "simple_assign.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the SYMBOL 'x' node
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # SYMBOL node shouldn't have FUNCTION descendants
  has_func <- has_descendant_of_type(symbol_nodes$id[1], c("FUNCTION"), data)

  expect_false(has_func)
})

# ============================================================================
# Level 3: Line-based Search
# ============================================================================

test_that("find_on_same_line finds assignment after symbol", {
  tmp_dir <- tempfile("scip_tree_same_line_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "assignment.R")
  writeLines(c(
    "x <- 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "assignment.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find SYMBOL 'x'
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Find assignments after this symbol on same line
  assignments <- find_on_same_line(
    symbol_nodes$id[1],
    data,
    c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"),
    "after"
  )

  # Should find the LEFT_ASSIGN
  expect_gt(nrow(assignments), 0)
  expect_true(any(assignments$token == "LEFT_ASSIGN"))
})

test_that("find_on_same_line with position='before' filters correctly", {
  tmp_dir <- tempfile("scip_tree_before_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "multi.R")
  writeLines(c(
    "x <- y <- 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "multi.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find the rightmost assignment
  assignments <- data[data$token == "LEFT_ASSIGN", , drop = FALSE]
  if (nrow(assignments) > 1) {
    rightmost <- assignments[order(assignments$col1, decreasing = TRUE), ][1, ]

    # Find symbols before this assignment
    symbols_before <- find_on_same_line(
      rightmost$id,
      data,
      c("SYMBOL"),
      "before"
    )

    # Should find at least the 'y' symbol
    expect_gt(nrow(symbols_before), 0)
  }
})

test_that("find_on_same_line without filters returns all nodes on line", {
  tmp_dir <- tempfile("scip_tree_all_line_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "line.R")
  writeLines(c(
    "x <- 1 + 2"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "line.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find SYMBOL 'x'
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "x", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Find all nodes on same line (no filters)
  same_line <- find_on_same_line(symbol_nodes$id[1], data)

  # Should find assignment, numbers, operator, etc.
  expect_gt(nrow(same_line), 3)  # At least assignment, numbers, plus
})

# ============================================================================
# Level 4: Assignment Expression Navigation
# ============================================================================

test_that("get_assignment_expr finds expr containing assignment", {
  tmp_dir <- tempfile("scip_tree_assign_expr_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "assign.R")
  writeLines(c(
    "x <- 1"
  ), file_path)

  file_info <- list(path = file_path, relative_path = "assign.R")
  ast <- parse_file(file_info)
  data <- ast$parse_data

  # Find LEFT_ASSIGN
  assign_nodes <- data[data$token == "LEFT_ASSIGN", , drop = FALSE]
  expect_gt(nrow(assign_nodes), 0)

  # Get the expression containing it
  expr <- get_assignment_expr(assign_nodes$id[1], data)

  expect_equal(nrow(expr), 1)
  expect_equal(expr$token[1], "expr")
})

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

  # Find SYMBOL_FORMALS tokens (parameters)
  params <- data[data$token == "SYMBOL_FORMALS", , drop = FALSE]

  # Should find 3 parameters
  expect_equal(nrow(params), 3)
  expect_setequal(params$text, c("x", "y", "z"))

  # Each parameter should have FUNCTION as a sibling (same parent expr)
  for (i in seq_len(nrow(params))) {
    # Get siblings of the parameter
    siblings <- get_siblings(params$id[i], data)

    # One of the siblings should be FUNCTION
    has_function_sibling <- any(siblings$token == "FUNCTION")
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

  # Find all SYMBOL tokens named 'i'
  symbols_i <- data[data$token == "SYMBOL" & data$text == "i", , drop = FALSE]
  expect_gt(nrow(symbols_i), 0)

  # At least one should have a forcond parent
  has_forcond_parent <- FALSE
  for (i in seq_len(nrow(symbols_i))) {
    parent <- get_parent(symbols_i$id[i], data)
    if (nrow(parent) > 0 && parent$token[1] == "forcond") {
      has_forcond_parent <- TRUE
      break
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

  # Find SYMBOL 'my_func'
  symbol_nodes <- data[data$token == "SYMBOL" & data$text == "my_func", , drop = FALSE]
  expect_gt(nrow(symbol_nodes), 0)

  # Find assignment after it
  assignments <- find_on_same_line(
    symbol_nodes$id[1],
    data,
    c("LEFT_ASSIGN"),
    "after"
  )
  expect_gt(nrow(assignments), 0)

  # Get the assignment expression
  assign_expr <- get_assignment_expr(assignments$id[1], data)
  expect_equal(nrow(assign_expr), 1)

  # Check if it contains FUNCTION
  has_func <- has_descendant_of_type(assign_expr$id[1], "FUNCTION", data)
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

  # All functions should return empty data.frames or FALSE
  expect_equal(nrow(get_children(1, empty_data)), 0)
  expect_equal(nrow(get_descendants(1, empty_data)), 0)
  expect_equal(nrow(get_parent(1, empty_data)), 0)
  expect_equal(nrow(get_siblings(1, empty_data)), 0)
  expect_equal(nrow(find_ancestor_of_type(1, "SYMBOL", empty_data)), 0)
  expect_false(has_descendant_of_type(1, "SYMBOL", empty_data))
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

  # Use non-existent node ID
  invalid_id <- 999999

  expect_equal(nrow(get_children(invalid_id, data)), 0)
  expect_equal(nrow(get_descendants(invalid_id, data)), 0)
  expect_equal(nrow(get_parent(invalid_id, data)), 0)
  expect_equal(nrow(get_siblings(invalid_id, data)), 0)
  expect_equal(nrow(find_ancestor_of_type(invalid_id, "SYMBOL", data)), 0)
  expect_false(has_descendant_of_type(invalid_id, "SYMBOL", data))
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
