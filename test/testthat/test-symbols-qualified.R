# Tests for package-qualified symbol detection (pkg::symbol and pkg:::symbol)
# This validates the symbols-qualified.R module implementation

# ============================================================================
# Level 1: Unit Tests for look_backward() Regex
# ============================================================================

test_that("look_backward detects exported function calls", {
  result <- look_backward("dplyr::mutate")
  
  expect_equal(result$package, "dplyr")
  expect_equal(result$accessor, "::")
  expect_equal(result$token, "mutate")
  expect_equal(result$full_token, "dplyr::mutate")
})

test_that("look_backward detects internal function calls", {
  result <- look_backward("pkg:::internal_helper")
  
  expect_equal(result$package, "pkg")
  expect_equal(result$accessor, ":::")
  expect_equal(result$token, "internal_helper")
})

test_that("look_backward handles unqualified symbols", {
  result <- look_backward("mutate")
  
  expect_equal(result$package, "")  # Empty, not NULL
  expect_equal(result$accessor, "")
  expect_equal(result$token, "mutate")
})

test_that("look_backward ignores $ accessor (not namespace)", {
  result <- look_backward("obj$field")
  
  expect_equal(result$package, "")
  expect_equal(result$accessor, "")
  # The negative lookbehind (?<!\\$) prevents matching after $
  # So token should be empty, not "field"
  expect_equal(result$token, "")
})

test_that("look_backward handles package names with dots", {
  result <- look_backward("data.table::fread")
  
  expect_equal(result$package, "data.table")
  expect_equal(result$accessor, "::")
  expect_equal(result$token, "fread")
})

test_that("look_backward handles multi-character symbols", {
  result <- look_backward("Matrix::rowSums")
  
  expect_equal(result$package, "Matrix")
  expect_equal(result$accessor, "::")
  expect_equal(result$token, "rowSums")
})

# ============================================================================
# Level 2: Edge Case Tests
# ============================================================================

test_that("look_backward handles nested namespace calls", {
  # From: purrr::map(data, dplyr::select, x, y)
  result1 <- look_backward("purrr::map")
  expect_equal(result1$package, "purrr")
  expect_equal(result1$token, "map")
  
  result2 <- look_backward("dplyr::select")
  expect_equal(result2$package, "dplyr")
  expect_equal(result2$token, "select")
})

test_that("look_backward rejects invalid package names", {
  # Package names must start with letter
  result <- look_backward("123pkg::func")
  expect_equal(result$package, "")  # Should not match
})

test_that("look_backward handles functions with underscores", {
  result <- look_backward("dplyr::mutate_at")
  expect_equal(result$package, "dplyr")
  expect_equal(result$token, "mutate_at")
})

test_that("look_backward handles S3 method-like names", {
  result <- look_backward("tibble::print.tbl")
  expect_equal(result$package, "tibble")
  expect_equal(result$token, "print.tbl")
})

test_that("look_backward handles symbols with dots", {
  result <- look_backward("base::as.data.frame")
  expect_equal(result$package, "base")
  expect_equal(result$token, "as.data.frame")
})

# ============================================================================
# Level 3: Integration Tests with Parse Data
# ============================================================================

test_that("extract_qualified_symbols detects all qualified calls in code", {
  tmp_dir <- tempfile("scip_r_qualified_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "qualified.R")
  writeLines(c(
    "# Test file with qualified calls",
    "df <- dplyr::mutate(data, x = 1)",
    "result <- Matrix::rowSums(sparse)",
    "helper <- pkg:::internal_func()",
    "local_func(unqualified)",
    "nested <- purrr::map(data, dplyr::select, cols)"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "qualified.R")
  ast <- parse_file(file_info)
  
  # Extract qualified symbols
  qualified <- extract_qualified_symbols(ast)
  
  # Should detect 5 qualified calls (2 in nested line)
  expect_equal(length(qualified), 5)
  
  # Check each detection
  packages <- sapply(qualified, function(x) x$package)
  expect_true("dplyr" %in% packages)
  expect_true("Matrix" %in% packages)
  expect_true("pkg" %in% packages)
  expect_true("purrr" %in% packages)
  
  # Check accessor types
  accessors <- sapply(qualified, function(x) x$accessor)
  expect_equal(sum(accessors == "::"), 4)   # 4 exported
  expect_equal(sum(accessors == ":::"), 1)  # 1 internal
  
  # Verify unqualified call NOT detected
  tokens <- sapply(qualified, function(x) x$token)
  expect_false("local_func" %in% tokens)
  expect_false("unqualified" %in% tokens)
})

# ============================================================================
# Level 4: Negative Controls
# ============================================================================

test_that("extract_qualified_symbols ignores non-namespace operators", {
  tmp_dir <- tempfile("scip_r_negative_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "negative.R")
  writeLines(c(
    "obj$field",           # $ accessor (not namespace)
    "x <- 1:10",           # : operator (sequence)
    "function(a, b) a + b", # No qualified calls
    "mutate(data, x = 1)"  # Unqualified call
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "negative.R")
  ast <- parse_file(file_info)
  qualified <- extract_qualified_symbols(ast)
  
  # Should find ZERO qualified namespace calls
  expect_equal(length(qualified), 0)
})

# ============================================================================
# Level 5: Performance Test
# ============================================================================

test_that("look_backward has acceptable performance", {
  # Generate 1000 qualified calls
  test_strings <- paste0("pkg", 1:1000, "::func", 1:1000)
  
  timing <- system.time({
    results <- lapply(test_strings, look_backward)
  })
  
  # Should process 1000 strings in < 100ms (0.1 seconds)
  expect_lt(timing["elapsed"], 0.1)
  
  # Verify all parsed correctly
  packages <- sapply(results, function(x) x$package)
  expect_true(all(startsWith(packages, "pkg")))
})

# ============================================================================
# Level 6: Real-World Pattern Tests
# ============================================================================

test_that("extract_qualified_symbols handles complex bioinformatics patterns", {
  tmp_dir <- tempfile("scip_r_bioinf_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  # Real bioinformatics pattern
  file_path <- file.path(tmp_dir, "deseq_analysis.R")
  writeLines(c(
    "library(DESeq2)",
    "",
    "dds <- DESeq2::DESeqDataSetFromMatrix(",
    "  countData = counts,",
    "  colData = metadata,",
    "  design = ~ condition",
    ")",
    "",
    "dds <- DESeq2::DESeq(dds)",
    "res <- DESeq2::results(dds, contrast = c('condition', 'treated', 'control'))",
    "",
    "# Use internal function (anti-pattern but occurs)",
    "norm_counts <- DESeq2:::getBaseMeansAndVariances(counts)"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "deseq_analysis.R")
  ast <- parse_file(file_info)
  qualified <- extract_qualified_symbols(ast)
  
  # Should detect 4 DESeq2 qualified calls
  deseq_calls <- Filter(function(x) x$package == "DESeq2", qualified)
  expect_equal(length(deseq_calls), 4)
  
  # Verify exported vs internal distinction
  exported <- Filter(function(x) x$accessor == "::", deseq_calls)
  internal <- Filter(function(x) x$accessor == ":::", deseq_calls)
  expect_equal(length(exported), 3)
  expect_equal(length(internal), 1)
  
  # Check function names
  func_names <- sapply(deseq_calls, function(x) x$token)
  expect_true("DESeqDataSetFromMatrix" %in% func_names)
  expect_true("getBaseMeansAndVariances" %in% func_names)
})
