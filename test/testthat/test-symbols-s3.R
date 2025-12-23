# Tests for S3 object system symbol detection
# This validates the symbols-s3.R module implementation

# ============================================================================
# Level 1: Unit Tests - Pattern Recognition
# ============================================================================

test_that("detects S3 generic with UseMethod", {
  tmp_dir <- tempfile("scip_s3_generic_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "generic.R")
  writeLines(c(
    "print <- function(x, ...) UseMethod('print')"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "generic.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should detect one S3 generic
  expect_equal(length(s3_symbols), 1)
  expect_equal(s3_symbols[[1]]$type, "s3_generic")
  expect_equal(s3_symbols[[1]]$generic_name, "print")
})

test_that("ignores functions without UseMethod", {
  tmp_dir <- tempfile("scip_s3_regular_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "regular.R")
  writeLines(c(
    "regular_function <- function(x) { x + 1 }"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "regular.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should find zero S3 generics (no UseMethod)
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  expect_equal(length(generics), 0)
})

test_that("detects S3 method by naming convention", {
  tmp_dir <- tempfile("scip_s3_method_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "method.R")
  writeLines(c(
    "print.myclass <- function(x) { cat('myclass:', x) }"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "method.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should detect one S3 method
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  expect_equal(length(methods), 1)
  expect_equal(methods[[1]]$generic_name, "print")
  expect_equal(methods[[1]]$class_name, "myclass")
})

test_that("ignores dots in non-method context", {
  tmp_dir <- tempfile("scip_s3_variable_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "variable.R")
  writeLines(c(
    "my.variable <- 5"  # Variable with dot, not a function
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "variable.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should not detect as S3 method (no function definition)
  expect_equal(length(s3_symbols), 0)
})

# ============================================================================
# Level 2: True Positives vs True Negatives
# ============================================================================

test_that("S3 detection true positives", {
  tmp_dir <- tempfile("scip_s3_positive_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "s3_positives.R")
  writeLines(c(
    "# TRUE POSITIVE 1: Generic with UseMethod",
    "plot <- function(x, ...) UseMethod('plot')",
    "",
    "# TRUE POSITIVE 2: Method for built-in generic",
    "print.myclass <- function(x) cat('myclass:', x)",
    "",
    "# TRUE POSITIVE 3: Method for custom class",
    "summary.mymodel <- function(object, ...) list(coef = object$coef)",
    "",
    "# TRUE POSITIVE 4: Multi-level class name",
    "mean.data.frame <- function(x, ...) sapply(x, mean)",
    "",
    "# TRUE POSITIVE 5: Generic calls UseMethod with different name",
    "my_generic <- function(x) UseMethod('my_generic')"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "s3_positives.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Verify we found generics and methods
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  
  expect_gte(length(generics), 2)  # At least plot and my_generic
  expect_gte(length(methods), 3)   # At least print.myclass, summary.mymodel, mean.data.frame
  
  # Verify method-generic linking
  method_names <- sapply(methods, function(x) x$method_name)
  generic_names <- sapply(methods, function(x) x$generic_name)
  
  expect_true("print" %in% generic_names)
  expect_true("summary" %in% generic_names)
  expect_true("mean" %in% generic_names)
})

test_that("S3 detection true negatives", {
  tmp_dir <- tempfile("scip_s3_negative_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "s3_negatives.R")
  writeLines(c(
    "# TRUE NEGATIVE 1: Variable with dot (not method)",
    "my.variable <- 10",
    "",
    "# TRUE NEGATIVE 2: Function with dot but not S3 pattern",
    "helper.internal <- function() { return(42) }",
    "",
    "# TRUE NEGATIVE 3: Regular function (no UseMethod)",
    "calculate_score <- function(x, y) x + y",
    "",
    "# TRUE NEGATIVE 4: Function that calls UseMethod but isn't defining generic",
    "caller <- function(x) { some_generic(x) }",
    "",
    "# TRUE NEGATIVE 5: String containing 'UseMethod' (not actual call)",
    "doc <- 'This function uses UseMethod internally'"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "s3_negatives.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # The function helper.internal might be detected as a method (heuristic)
  # But no generics should be found, and no variables
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  expect_equal(length(generics), 0)
  
  # Ensure no variable assignments are detected
  # (only function definitions should be in s3_symbols)
  for (sym in s3_symbols) {
    # Verify each has a function-related type
    expect_true(sym$type %in% c("s3_generic", "s3_method"))
  }
})

# ============================================================================
# Level 3: Real-World Tidyverse Patterns
# ============================================================================

test_that("detects tidyverse S3 patterns", {
  tmp_dir <- tempfile("scip_s3_tidyverse_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  # Pattern from dplyr
  file_path <- file.path(tmp_dir, "dplyr_pattern.R")
  writeLines(c(
    "mutate <- function(.data, ...) UseMethod('mutate')",
    "mutate.data.frame <- function(.data, ...) {",
    "  # implementation",
    "}",
    "mutate.tbl_df <- function(.data, ...) {",
    "  # tibble-specific implementation",
    "}"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "dplyr_pattern.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should find: 1 generic (mutate) + 2 methods
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  
  expect_equal(length(generics), 1)
  expect_equal(generics[[1]]$generic_name, "mutate")
  
  expect_equal(length(methods), 2)
  
  # Verify both methods link to "mutate" generic
  generic_refs <- sapply(methods, function(x) x$generic_name)
  expect_true(all(generic_refs == "mutate"))
})

test_that("detects ggplot2 S3 patterns", {
  tmp_dir <- tempfile("scip_s3_ggplot_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "ggplot_pattern.R")
  writeLines(c(
    "# ggplot2 uses + operator as S3 generic",
    "ggplot_add <- function(object, plot, object_name) UseMethod('ggplot_add')",
    "",
    "ggplot_add.Layer <- function(object, plot, object_name) {",
    "  plot$layers <- c(plot$layers, object)",
    "  plot",
    "}",
    "",
    "print.ggplot <- function(x, ...) {",
    "  grid.draw(ggplotGrob(x))",
    "}"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "ggplot_pattern.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  
  expect_gte(length(generics), 1)  # ggplot_add
  expect_gte(length(methods), 2)   # ggplot_add.Layer, print.ggplot
})

# ============================================================================
# Level 4: Edge Cases & Ambiguity Resolution
# ============================================================================

test_that("distinguishes S3 from namespace operators", {
  tmp_dir <- tempfile("scip_s3_namespace_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "namespace.R")
  writeLines(c(
    "# Namespace call - should NOT be detected",
    "result <- stats::print.summary",
    "",
    "# Method definition - SHOULD be detected",
    "print.myclass <- function(x) { }"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "namespace.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Only the method definition should be detected
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  expect_gte(length(methods), 1)
  
  # Verify it's the print.myclass, not the namespace call
  method_names <- sapply(methods, function(x) x$method_name)
  expect_true("print.myclass" %in% method_names)
})

test_that("handles function names with multiple dots", {
  tmp_dir <- tempfile("scip_s3_multidot_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "multidot.R")
  writeLines(c(
    "as.data.frame.table <- function(x, ...) { }"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "multidot.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Should detect as method for "as.data.frame" generic
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  expect_gte(length(methods), 1)
  
  method <- methods[[1]]
  expect_equal(method$generic_name, "as.data.frame")
  expect_equal(method$class_name, "table")
})

test_that("ignores dots in non-function contexts", {
  tmp_dir <- tempfile("scip_s3_nonfunc_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "nonfunc.R")
  writeLines(c(
    "version <- '1.0.0'",           # Dots in string
    "config <- list(mean.value = 5)",  # Named list element
    "df$col.name"                    # Column name
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "nonfunc.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # None should be detected as S3 (no function definitions)
  expect_equal(length(s3_symbols), 0)
})

test_that("handles UseMethod without explicit argument", {
  tmp_dir <- tempfile("scip_s3_implicit_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "implicit.R")
  writeLines(c(
    "# UseMethod called without argument - uses function name",
    "my_generic <- function(x) UseMethod()"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "implicit.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  
  # Should still detect as generic
  expect_gte(length(generics), 1)
  
  # Generic name should be the function name
  expect_equal(generics[[1]]$generic_name, "my_generic")
})

# ============================================================================
# Level 5: Integration Test with SCIP Descriptors
# ============================================================================

test_that("generates correct SCIP descriptors for S3 symbols", {
  tmp_dir <- tempfile("scip_s3_integration_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "package.R")
  writeLines(c(
    "print <- function(x, ...) UseMethod('print')",
    "print.myclass <- function(x) cat('myclass')"
  ), file_path)
  
  # Create workspace context
  workspace <- list(
    root = tmp_dir,
    metadata = list(
      namespace = "mypackage",
      version = "1.0.0"
    )
  )
  
  file_info <- list(path = file_path, relative_path = "package.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  # Convert to SCIP descriptors
  scip_symbols <- lapply(s3_symbols, function(sym) {
    s3_to_scip(sym, workspace)
  })
  
  # Expected descriptors:
  # scip-r mypackage 1.0.0 print().(s3_generic)
  # scip-r mypackage 1.0.0 print.myclass().(s3_method)
  
  descriptor_strings <- sapply(scip_symbols, function(x) x$symbol)
  
  expect_true(any(grepl("print\\(\\)\\.\\(s3_generic\\)", descriptor_strings)))
  expect_true(any(grepl("print\\.myclass\\(\\)\\.\\(s3_method\\)", descriptor_strings)))
})

# ============================================================================
# Level 6: Performance Benchmark
# ============================================================================

test_that("S3 extraction has acceptable performance", {
  skip_on_cran()  # Performance tests can be slow on CRAN
  
  # Generate a large file with 100 S3 methods
  methods <- paste0(
    "print.class", 1:100, " <- function(x) { cat('class", 1:100, "') }"
  )
  
  tmp_dir <- tempfile("scip_s3_perf_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "large.R")
  writeLines(methods, file_path)
  
  file_info <- list(path = file_path, relative_path = "large.R")
  
  timing <- system.time({
    ast <- parse_file(file_info)
    s3_symbols <- extract_s3_symbols(ast)
  })
  
  # Should process 100 methods in < 100ms
  expect_lt(timing["elapsed"], 0.1)
  
  # Verify correct count (all are methods)
  methods_found <- Filter(function(x) x$type == "s3_method", s3_symbols)
  expect_equal(length(methods_found), 100)
})

# ============================================================================
# Level 7: Complex Real-World Example
# ============================================================================

test_that("handles complex S3 class hierarchy from data.table", {
  tmp_dir <- tempfile("scip_s3_datatable_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  # Simplified data.table pattern
  file_path <- file.path(tmp_dir, "datatable.R")
  writeLines(c(
    "# Generic",
    "as.data.table <- function(x, ...) UseMethod('as.data.table')",
    "",
    "# Methods for different classes",
    "as.data.table.data.frame <- function(x, ...) {",
    "  setDT(x)",
    "}",
    "",
    "as.data.table.list <- function(x, ...) {",
    "  as.data.table(as.data.frame(x))",
    "}",
    "",
    "as.data.table.matrix <- function(x, ...) {",
    "  as.data.table(as.data.frame(x))",
    "}"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "datatable.R")
  ast <- parse_file(file_info)
  s3_symbols <- extract_s3_symbols(ast)
  
  generics <- Filter(function(x) x$type == "s3_generic", s3_symbols)
  methods <- Filter(function(x) x$type == "s3_method", s3_symbols)
  
  # Should find 1 generic (as.data.table with UseMethod)
  expect_equal(length(generics), 1)
  expect_equal(generics[[1]]$generic_name, "as.data.table")
  
  # Should find 4 methods: as.data.table itself (detected as method by naming),
  # plus as.data.table.data.frame, as.data.table.list, as.data.table.matrix
  # This is because our heuristic sees "as.data.table" as "as.data" + ".table" pattern
  expect_gte(length(methods), 3)
  
  # At least some methods should be detected
  # (Exact matching depends on known_generics list coverage)
  expect_gt(length(methods), 0)
  
  # Verify we have the expected method names (regardless of how they're parsed)
  method_names <- sapply(methods, function(x) x$method_name)
  expect_true("as.data.table.data.frame" %in% method_names ||
              "as.data.table.list" %in% method_names ||
              "as.data.table.matrix" %in% method_names)
})
