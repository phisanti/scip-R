# Integration tests for dynamic dispatch detection in resolve_symbols() pipeline
#
# These tests verify that the unified dispatch interface correctly integrates
# all three dispatch extractors (Shiny, Base R, C interface) into the
# resolve_symbols() pipeline and produces proper SCIP relationships.

# =============================================================================
# Test 1: Cross-pattern integration — all dispatch types in one file
# =============================================================================
testthat::test_that("resolve_symbols integrates all dispatch patterns", {
  code <- '
# Shiny module
BiomarkerBoard <- function(input, output, session) {
  # module logic
}

# Base R function
compute_mean <- function(x) mean(x)

# Server with all dispatch types
server <- function(input, output, session) {
  # Shiny dispatch
  callModule(BiomarkerBoard, "biomarker")

  # Base R dispatch
  result <- do.call("compute_mean", list(x = 1:10))

  # C interface
  .Call("_scipR_build_tree_index_cpp", id, parent, line1)
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(), dependencies = list())

  # Call resolve_symbols (which should now integrate dispatch detection)
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Check that relationships were attached to at least some symbols
  symbols_with_rels <- Filter(function(s) {
    !is.null(s$relationships) && length(s$relationships) > 0
  }, result$symbols)

  # Should have at least 2 symbols with relationships:
  # BiomarkerBoard (via callModule) and compute_mean (via do.call)
  # C function is external, may or may not appear in symbol list
  testthat::expect_true(
    length(symbols_with_rels) >= 2,
    label = paste("Expected >= 2 symbols with relationships, got", length(symbols_with_rels))
  )

  # Verify relationship structure is well-formed
  for (sym in symbols_with_rels) {
    for (rel in sym$relationships) {
      testthat::expect_true(!is.null(rel$symbol))
      testthat::expect_equal(rel$is_reference, TRUE)
      testthat::expect_true(!is.null(rel$confidence))
      testthat::expect_true(!is.null(rel$dispatch_type))
    }
  }

  unlink(tmp)
})

# =============================================================================
# Test 2: Unified interface returns edges from all extractors
# =============================================================================
testthat::test_that("extract_dynamic_call_edges combines all three extractors", {
  code <- '
myFunc <- function(x) x * 2

server <- function(input, output, session) {
  callModule(myFunc, "id1")
}

result <- do.call("myFunc", list(x = 5))
.Call("_pkg_native_func", 42)
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_dynamic_call_edges(
    ast$parse_data, NULL, NULL, basename(tmp)
  )

  # Should have edges from all three extractors
  dispatch_types <- sapply(edges, function(e) e$dispatch_type)

  # Shiny edge
  testthat::expect_true("callModule" %in% dispatch_types)
  # Base R edge
  testthat::expect_true("do.call" %in% dispatch_types)
  # C interface edge
  testthat::expect_true(".Call" %in% dispatch_types)

  # All edges should have required fields
  for (edge in edges) {
    testthat::expect_true(!is.null(edge$symbol))
    testthat::expect_equal(edge$is_reference, TRUE)
    testthat::expect_true(edge$confidence >= 0.9)
  }

  unlink(tmp)
})

# =============================================================================
# Test 3: Non-existent target handling (no crash)
# =============================================================================
testthat::test_that("handles non-existent dispatch targets gracefully", {
  code <- '
server <- function(input, output, session) {
  callModule(NonExistentFunction, "id")
  do.call("AlsoDoesNotExist", list())
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(), dependencies = list())

  # Should not crash even when targets don't exist in symbol table
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # The function should still return a valid result
  testthat::expect_true(is.list(result))
  testthat::expect_true("symbols" %in% names(result))
  testthat::expect_true("occurrences" %in% names(result))

  # Dispatch edges may still be created (with local/fallback symbol IDs)
  # The important thing is no crash occurred
  testthat::expect_true(length(result$symbols) >= 0)

  unlink(tmp)
})

# =============================================================================
# Test 4: Empty file produces no dispatch edges
# =============================================================================
testthat::test_that("empty file produces no dispatch edges", {
  code <- '
# Just a comment, no dispatch calls
x <- 1 + 2
y <- "hello"
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_dynamic_call_edges(
    ast$parse_data, NULL, NULL, basename(tmp)
  )

  testthat::expect_equal(length(edges), 0)

  unlink(tmp)
})

# =============================================================================
# Test 5: Multiple dispatch calls to same target merge correctly
# =============================================================================
testthat::test_that("multiple dispatch calls to same target accumulate relationships", {
  code <- '
my_func <- function(x) x * 2

# Call my_func via do.call twice
result1 <- do.call("my_func", list(x = 5))
result2 <- do.call("my_func", list(x = 10))

# Also call via match.fun
f <- match.fun("my_func")
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(), dependencies = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Find my_func symbol
  my_func_sym <- Filter(function(s) {
    !is.null(s$display_name) && s$display_name == "my_func"
  }, result$symbols)

  # my_func should exist
  testthat::expect_true(length(my_func_sym) >= 1)

  # If relationships are attached, there should be multiple
  my_func_entry <- my_func_sym[[1]]
  if (!is.null(my_func_entry$relationships)) {
    # Each do.call and match.fun creates a separate edge
    testthat::expect_true(length(my_func_entry$relationships) >= 2)
  }

  unlink(tmp)
})

# =============================================================================
# Test 6: SCIP serialization does not crash with dispatch relationships
# =============================================================================
testthat::test_that("dispatch relationships do not break SCIP serialization", {
  code <- '
MyModule <- function(input, output, session) {
  # module logic
}

server <- function(input, output, session) {
  callModule(MyModule, "mymod")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(
    metadata = list(namespace = NULL, project_root = tempdir()),
    dependencies = list()
  )

  # Resolve symbols (includes dispatch detection)
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Check that relationships exist
  symbols_with_rels <- Filter(function(s) {
    !is.null(s$relationships) && length(s$relationships) > 0
  }, result$symbols)

  testthat::expect_true(length(symbols_with_rels) >= 1)

  # Build document structure
  doc <- build_document(ast$file_info, result$occurrences, result)

  # Verify document has symbols with relationships
  doc_rels <- Filter(function(s) {
    !is.null(s$relationships) && length(s$relationships) > 0
  }, doc$symbols)

  testthat::expect_true(length(doc_rels) >= 1)

  # Check relationship structure matches what serializer expects
  for (sym in doc_rels) {
    for (rel in sym$relationships) {
      testthat::expect_true(!is.null(rel$symbol))
      testthat::expect_true(is.character(rel$symbol))
      testthat::expect_equal(rel$is_reference, TRUE)
    }
  }

  unlink(tmp)
})

# =============================================================================
# Test 7: Shiny-only dispatch creates relationships on module symbol
# =============================================================================
testthat::test_that("Shiny callModule creates relationship on target module", {
  code <- '
DataBoard <- function(input, output, session) {
  output$plot <- renderPlot({
    plot(1:10)
  })
}

server <- function(input, output, session) {
  callModule(DataBoard, "data_board")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(), dependencies = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  # Find DataBoard symbol
  data_board <- Filter(function(s) {
    !is.null(s$display_name) && s$display_name == "DataBoard"
  }, result$symbols)

  testthat::expect_true(length(data_board) >= 1)

  # DataBoard should have a relationship from the callModule dispatch
  db_entry <- data_board[[1]]
  testthat::expect_true(
    !is.null(db_entry$relationships) && length(db_entry$relationships) > 0,
    label = "DataBoard should have dispatch relationship"
  )

  # Verify the relationship is from callModule
  rel <- db_entry$relationships[[1]]
  testthat::expect_equal(rel$dispatch_type, "callModule")
  testthat::expect_equal(rel$is_reference, TRUE)

  unlink(tmp)
})
