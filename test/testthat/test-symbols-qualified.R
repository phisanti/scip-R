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

# ============================================================================
# Level 7: Anti-Regression Tests for External Symbol IDs in Occurrences
# ============================================================================
# BUG FIX: pkg::func() calls emitted local symbol IDs instead of external IDs
# Root cause: token loop did not detect NS_GET siblings, so qualified calls
# fell through to local fallback in resolve_symbol_id_fast()
# Anti-regression tests below verify occurrence symbol IDs match external format

test_that("dplyr::filter() occurrence emits external symbol ID", {
  tmp_dir <- tempfile("scip_r_ext_id_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "qualified_call.R")
  writeLines("result <- dplyr::filter(df, x > 1)", file_path)

  file_info <- list(path = file_path, relative_path = "qualified_call.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  result <- resolve_symbols(ast, workspace)

  # Find the occurrence for "filter"
  filter_occs <- Filter(function(occ) grepl("filter", occ$symbol), result$occurrences)
  expect_true(length(filter_occs) >= 1)

  # The symbol ID must be external format: scip-r r dplyr 0.0.0 filter().
  filter_sym <- filter_occs[[1]]$symbol
  expect_equal(filter_sym, "scip-r r dplyr 0.0.0 filter().")
})

test_that("shiny::observeEvent() occurrence emits external symbol ID", {
  tmp_dir <- tempfile("scip_r_ext_id_shiny_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "shiny_call.R")
  writeLines("shiny::observeEvent(input$x, { print('hi') })", file_path)

  file_info <- list(path = file_path, relative_path = "shiny_call.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  result <- resolve_symbols(ast, workspace)

  observe_occs <- Filter(function(occ) grepl("observeEvent", occ$symbol), result$occurrences)
  expect_true(length(observe_occs) >= 1)

  observe_sym <- observe_occs[[1]]$symbol
  expect_equal(observe_sym, "scip-r r shiny 0.0.0 observeEvent().")
})

test_that("triple-colon pkg:::internal_func() emits external symbol ID", {
  tmp_dir <- tempfile("scip_r_ext_id_triple_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "triple_colon.R")
  writeLines("pkg:::internal_func()", file_path)

  file_info <- list(path = file_path, relative_path = "triple_colon.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  result <- resolve_symbols(ast, workspace)

  func_occs <- Filter(function(occ) grepl("internal_func", occ$symbol), result$occurrences)
  expect_true(length(func_occs) >= 1)

  func_sym <- func_occs[[1]]$symbol
  expect_equal(func_sym, "scip-r r pkg 0.0.0 internal_func().")
})

test_that("bare filter(x) without :: does NOT get external symbol ID", {
  tmp_dir <- tempfile("scip_r_bare_call_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "bare_call.R")
  writeLines("filter(df, x > 1)", file_path)

  file_info <- list(path = file_path, relative_path = "bare_call.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  result <- resolve_symbols(ast, workspace)

  filter_occs <- Filter(function(occ) grepl("filter", occ$symbol), result$occurrences)
  expect_true(length(filter_occs) >= 1)

  # Must NOT start with "scip-r r dplyr" — should be local fallback
  filter_sym <- filter_occs[[1]]$symbol
  expect_false(grepl("^scip-r r dplyr", filter_sym))
  # Should be a local symbol (local prefix)
  expect_true(grepl("^local", filter_sym))
})

test_that("external_symbols format includes 'r' manager field", {
  # Verify build_external_symbol_messages produces correct format
  externals <- list(
    list(
      package = "dplyr",
      version = "1.1.0",
      symbols = list("filter", "mutate")
    )
  )
  msgs <- build_external_symbol_messages(externals)
  expect_equal(length(msgs), 2)

  # Both should have "scip-r r dplyr" format (with 'r' manager)
  sym1 <- msgs[[1]]$symbol
  sym2 <- msgs[[2]]$symbol
  expect_true(grepl("^scip-r r dplyr 1\\.1\\.0", sym1))
  expect_true(grepl("^scip-r r dplyr 1\\.1\\.0", sym2))
  # Specifically should NOT be "scip-r dplyr" (missing 'r')
  expect_false(grepl("^scip-r dplyr", sym1))
})

# ============================================================================
# Level 8: Anti-Regression Tests for Same-Package Qualified Calls (Issue 1)
# ============================================================================
# BUG FIX: myPkg::myFunc() from within myPkg produced version "0.0.0" instead
# of the actual package version from the definitions_env.
# Root cause: the qualified_pkg early-return in resolve_symbol_id_fast()
# hardcoded "0.0.0" without checking definitions_env first.

test_that("self-qualified call uses definition's version, not 0.0.0", {
  tmp_dir <- tempfile("scip_r_self_qual_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a file that calls myPkg::myFunc() — same package as the definition
  file_path <- file.path(tmp_dir, "caller.R")
  writeLines("result <- myPkg::myFunc(x)", file_path)

  file_info <- list(path = file_path, relative_path = "caller.R")
  ast <- parse_file(file_info)

  # Set up a workspace that looks like package "myPkg" with version "1.2.3"
  workspace <- list(
    metadata = list(namespace = list(package = "myPkg")),
    dependencies = list(myPkg = "1.2.3")
  )

  # Build a symbol table with myFunc defined in the package
  definitions_env <- new.env(parent = emptyenv())
  definitions_env[["myFunc"]] <- list(
    symbol_id = "scip-r r myPkg 1.2.3 myFunc().",
    file = "R/funcs.R",
    kind = "function"
  )
  symbol_table <- list(
    definitions_env = definitions_env,
    package_name = "myPkg",
    package_version = "1.2.3"
  )

  result <- resolve_symbols(ast, workspace, symbol_table = symbol_table)

  # Find the occurrence for "myFunc"
  func_occs <- Filter(function(occ) grepl("myFunc", occ$symbol), result$occurrences)
  expect_true(length(func_occs) >= 1)

  # The symbol ID must use the real version from definitions_env, NOT "0.0.0"
  func_sym <- func_occs[[1]]$symbol
  expect_equal(func_sym, "scip-r r myPkg 1.2.3 myFunc().")
  expect_false(grepl("0\\.0\\.0", func_sym),
               info = "Self-qualified call should NOT use fallback version 0.0.0")
})

test_that("cross-package qualified call still uses 0.0.0 when version unknown", {
  tmp_dir <- tempfile("scip_r_cross_qual_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "caller.R")
  writeLines("result <- otherPkg::otherFunc(x)", file_path)

  file_info <- list(path = file_path, relative_path = "caller.R")
  ast <- parse_file(file_info)

  workspace <- list(metadata = list(namespace = NULL))

  # definitions_env has NO entry for otherFunc
  definitions_env <- new.env(parent = emptyenv())
  symbol_table <- list(
    definitions_env = definitions_env,
    package_name = "myPkg",
    package_version = "1.0.0"
  )

  result <- resolve_symbols(ast, workspace, symbol_table = symbol_table)

  func_occs <- Filter(function(occ) grepl("otherFunc", occ$symbol), result$occurrences)
  expect_true(length(func_occs) >= 1)

  # Cross-package call with unknown version should still use "0.0.0"
  func_sym <- func_occs[[1]]$symbol
  expect_equal(func_sym, "scip-r r otherPkg 0.0.0 otherFunc().")
})

# ============================================================================
# Level 9: Anti-Regression Tests for External Symbol Kind Suffix (Issue 2)
# ============================================================================
# BUG FIX: build_external_symbol_messages() hardcoded "()" suffix for all
# external symbols. Non-function symbols like pkg::CONSTANT should get "."
# suffix, not "().".

test_that("external symbol message uses variable suffix for non-function symbols", {
  externals <- list(
    list(
      package = "myPkg",
      version = "1.0.0",
      symbols = list(
        list(name = "SOME_CONSTANT", kind = "variable"),
        list(name = "myFunc", kind = "function")
      )
    )
  )
  msgs <- build_external_symbol_messages(externals)
  expect_equal(length(msgs), 2)

  # Variable should get "." suffix (no parentheses)
  var_msg <- msgs[[1]]$symbol
  expect_equal(var_msg, "scip-r r myPkg 1.0.0 SOME_CONSTANT.")
  expect_false(grepl("\\(\\)", var_msg),
               info = "Variable symbols must NOT have () suffix")

  # Function should still get "()" suffix
  func_msg <- msgs[[2]]$symbol
  expect_equal(func_msg, "scip-r r myPkg 1.0.0 myFunc().")
})

test_that("external symbol message defaults to function for string-only format", {
  # Old format: symbols as plain strings (no kind info)
  externals <- list(
    list(
      package = "dplyr",
      version = "1.1.0",
      symbols = list("filter", "mutate")
    )
  )
  msgs <- build_external_symbol_messages(externals)
  expect_equal(length(msgs), 2)

  # String-only format should default to function suffix "()"
  expect_equal(msgs[[1]]$symbol, "scip-r r dplyr 1.1.0 filter().")
  expect_equal(msgs[[2]]$symbol, "scip-r r dplyr 1.1.0 mutate().")
})

test_that("pkg::CONSTANT occurrence produces variable-style symbol ID", {
  tmp_dir <- tempfile("scip_r_const_qual_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Use a non-function qualified reference (SYMBOL token, not SYMBOL_FUNCTION_CALL)
  file_path <- file.path(tmp_dir, "const_ref.R")
  writeLines("x <- config::MAX_VALUE", file_path)

  file_info <- list(path = file_path, relative_path = "const_ref.R")
  ast <- parse_file(file_info)
  workspace <- list(metadata = list(namespace = NULL))
  result <- resolve_symbols(ast, workspace)

  # Find the occurrence for "MAX_VALUE"
  const_occs <- Filter(function(occ) grepl("MAX_VALUE", occ$symbol), result$occurrences)
  expect_true(length(const_occs) >= 1)

  # Variable-style symbol: "scip-r r config 0.0.0 MAX_VALUE."
  # NOT function-style: "scip-r r config 0.0.0 MAX_VALUE()."
  const_sym <- const_occs[[1]]$symbol
  expect_equal(const_sym, "scip-r r config 0.0.0 MAX_VALUE.")
  expect_false(grepl("\\(\\)", const_sym),
               info = "Non-function qualified ref must NOT have () suffix")
})
