# Tests for cross-file symbol resolution.
# These tests verify that function calls resolve to the same global symbol ID
# as their definitions, enabling "go to definition" across files.

testthat::test_that("cross-file function calls use same symbol ID as definitions", {
  tmp_dir <- tempfile("scip_crossfile_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create utils.R with a function definition
  writeLines(c(
    "formatDate <- function(date) {",
    "  format(date, \"%Y-%m-%d\")",
    "}"
  ), file.path(tmp_dir, "R", "utils.R"))

  # Create main.R that calls formatDate
  writeLines(c(
    "process_data <- function(x) {",
    "  result <- formatDate(Sys.Date())",
    "  return(result)",
    "}"
  ), file.path(tmp_dir, "R", "main.R"))

  # Create DESCRIPTION
  writeLines(c(
    "Package: testpkg",
    "Version: 1.0.0",
    "Title: Test Package"
  ), file.path(tmp_dir, "DESCRIPTION"))

  # Create NAMESPACE
  writeLines(c(
    "export(formatDate)",
    "export(process_data)"
  ), file.path(tmp_dir, "NAMESPACE"))

  # Analyze workspace
  workspace <- analyze_workspace(tmp_dir)

  # Discover and parse files
  plan <- discover_files(workspace)

  # Phase 1: Parse files
  parsed_files <- list()
  file_infos <- list()
  for (file_info in plan$files) {
    ast <- parse_file(file_info)
    if (!is.null(ast)) {
      parsed_files[[length(parsed_files) + 1]] <- ast
      file_infos[[length(file_infos) + 1]] <- file_info
    }
  }

  # Phase 2: Build symbol table
  symbol_table <- build_symbol_table(parsed_files, workspace)

  # Verify symbol table contains both definitions
  expect_equal(length(symbol_table$definitions), 2)
  expect_true("formatDate" %in% names(symbol_table$definitions))
  expect_true("process_data" %in% names(symbol_table$definitions))

  # Get the expected global symbol ID for formatDate
  format_date_id <- symbol_table$definitions[["formatDate"]]$symbol_id
  expect_true(grepl("^scip-r r", format_date_id))
  expect_true(grepl("formatDate\\(\\)\\.$", format_date_id))

  # Phase 3: Resolve symbols for each file
  results <- list()
  for (i in seq_along(parsed_files)) {
    ast <- parsed_files[[i]]
    file_info <- file_infos[[i]]
    relative_path <- file_info$relative_path
    tree_index <- symbol_table$tree_indices[[relative_path]]
    analysis <- resolve_symbols(ast, workspace, track_externals = TRUE,
                                symbol_table = symbol_table, tree_index = tree_index)
    results[[relative_path]] <- analysis
  }

  # Find formatDate definition occurrence in utils.R
  utils_occs <- results[["R/utils.R"]]$occurrences
  format_date_def <- Find(function(occ) {
    grepl("formatDate", occ$symbol) && occ$role == "definition"
  }, utils_occs)

  expect_false(is.null(format_date_def))
  expect_equal(format_date_def$symbol, format_date_id)

  # Find formatDate reference occurrence in main.R
  main_occs <- results[["R/main.R"]]$occurrences
  format_date_ref <- Find(function(occ) {
    grepl("formatDate", occ$symbol) && occ$role == "reference"
  }, main_occs)

  expect_false(is.null(format_date_ref))
  # CRITICAL: Reference should use SAME symbol ID as definition
  expect_equal(format_date_ref$symbol, format_date_id)
})

testthat::test_that("cross-file resolution works for multiple functions", {
  tmp_dir <- tempfile("scip_crossfile_multi_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create helpers.R with multiple function definitions
  writeLines(c(
    "helper_a <- function(x) x + 1",
    "helper_b <- function(x) x * 2",
    "helper_c <- function(x) x - 1"
  ), file.path(tmp_dir, "R", "helpers.R"))

  # Create main.R that calls all helpers
  writeLines(c(
    "main_func <- function(x) {",
    "  step1 <- helper_a(x)",
    "  step2 <- helper_b(step1)",
    "  step3 <- helper_c(step2)",
    "  return(step3)",
    "}"
  ), file.path(tmp_dir, "R", "main.R"))

  # Create DESCRIPTION
  writeLines(c(
    "Package: multipkg",
    "Version: 2.0.0",
    "Title: Multi-Function Package"
  ), file.path(tmp_dir, "DESCRIPTION"))

  # Analyze and process
  workspace <- analyze_workspace(tmp_dir)
  plan <- discover_files(workspace)

  parsed_files <- list()
  file_infos <- list()
  for (file_info in plan$files) {
    ast <- parse_file(file_info)
    if (!is.null(ast)) {
      parsed_files[[length(parsed_files) + 1]] <- ast
      file_infos[[length(file_infos) + 1]] <- file_info
    }
  }

  symbol_table <- build_symbol_table(parsed_files, workspace)

  # Verify all helper functions are in symbol table
  expect_true("helper_a" %in% names(symbol_table$definitions))
  expect_true("helper_b" %in% names(symbol_table$definitions))
  expect_true("helper_c" %in% names(symbol_table$definitions))

  # Resolve symbols for main.R
  main_idx <- which(sapply(file_infos, function(f) f$relative_path) == "R/main.R")
  ast <- parsed_files[[main_idx]]
  file_info <- file_infos[[main_idx]]
  tree_index <- symbol_table$tree_indices[["R/main.R"]]
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE,
                              symbol_table = symbol_table, tree_index = tree_index)

  # Verify each helper reference uses the correct global symbol ID
  for (helper in c("helper_a", "helper_b", "helper_c")) {
    expected_id <- symbol_table$definitions[[helper]]$symbol_id
    ref_occ <- Find(function(occ) {
      grepl(helper, occ$symbol) && occ$role == "reference"
    }, analysis$occurrences)

    expect_false(is.null(ref_occ))
    expect_equal(ref_occ$symbol, expected_id)
  }
})

testthat::test_that("local variables inside functions do NOT get global IDs", {
  tmp_dir <- tempfile("scip_localtest_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a file with local variables inside function
  writeLines(c(
    "my_func <- function(myarg) {",
    "  tempval <- myarg + 1",
    "  otherval <- tempval * 2",
    "  return(otherval)",
    "}"
  ), file.path(tmp_dir, "R", "main.R"))

  # Create DESCRIPTION
  writeLines(c(
    "Package: localpkg",
    "Version: 1.0.0",
    "Title: Local Package"
  ), file.path(tmp_dir, "DESCRIPTION"))

  workspace <- analyze_workspace(tmp_dir)
  plan <- discover_files(workspace)

  parsed_files <- list()
  file_infos <- list()
  for (file_info in plan$files) {
    ast <- parse_file(file_info)
    if (!is.null(ast)) {
      parsed_files[[length(parsed_files) + 1]] <- ast
      file_infos[[length(file_infos) + 1]] <- file_info
    }
  }

  symbol_table <- build_symbol_table(parsed_files, workspace)

  # my_func should be in symbol table with global ID
  expect_true("my_func" %in% names(symbol_table$definitions))
  expect_true(grepl("^scip-r r", symbol_table$definitions[["my_func"]]$symbol_id))

  # Local variables (myarg, tempval, otherval) should NOT be in symbol table
  expect_false("myarg" %in% names(symbol_table$definitions))
  expect_false("tempval" %in% names(symbol_table$definitions))
  expect_false("otherval" %in% names(symbol_table$definitions))

  # Resolve symbols
  ast <- parsed_files[[1]]
  tree_index <- symbol_table$tree_indices[["R/main.R"]]
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE,
                              symbol_table = symbol_table, tree_index = tree_index)

  # Verify local variables have local symbol IDs (not global)
  # Match exact name at end of symbol ID: #<name>$
  for (local_name in c("myarg", "tempval", "otherval")) {
    pattern <- paste0("#", local_name, "$")
    local_occs <- Filter(function(occ) grepl(pattern, occ$symbol), analysis$occurrences)
    # Use expect_true with length check for compatibility
    expect_true(length(local_occs) > 0)

    for (occ in local_occs) {
      expect_true(grepl("^local ", occ$symbol))
    }
  }
})

testthat::test_that("base R function calls get base package symbol IDs", {
  tmp_dir <- tempfile("scip_base_funcs_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a file using base R functions
  writeLines(c(
    "my_func <- function(x) {",
    "  n <- length(x)",
    "  s <- sum(x)",
    "  m <- mean(x)",
    "  return(c(n, s, m))",
    "}"
  ), file.path(tmp_dir, "R", "main.R"))

  # Create DESCRIPTION
  writeLines(c(
    "Package: basepkg",
    "Version: 1.0.0",
    "Title: Base Package"
  ), file.path(tmp_dir, "DESCRIPTION"))

  workspace <- analyze_workspace(tmp_dir)
  plan <- discover_files(workspace)

  parsed_files <- list()
  for (file_info in plan$files) {
    ast <- parse_file(file_info)
    if (!is.null(ast)) parsed_files[[length(parsed_files) + 1]] <- ast
  }

  symbol_table <- build_symbol_table(parsed_files, workspace)

  ast <- parsed_files[[1]]
  tree_index <- symbol_table$tree_indices[["R/main.R"]]
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE,
                              symbol_table = symbol_table, tree_index = tree_index)

  # Check base R functions have base package symbol IDs
  for (base_func in c("length", "sum", "mean", "c", "return")) {
    ref_occ <- Find(function(occ) {
      grepl(paste0(base_func, "\\(\\)\\."), occ$symbol) && occ$role == "reference"
    }, analysis$occurrences)

    expect_false(is.null(ref_occ))
    expect_true(grepl("scip-r r base", ref_occ$symbol))
  }
})

testthat::test_that("symbol table definitions_env provides O(1) lookup", {
  tmp_dir <- tempfile("scip_env_lookup_")
  dir.create(tmp_dir)
  dir.create(file.path(tmp_dir, "R"))
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  # Create a file with multiple functions
  writeLines(c(
    "func_a <- function() 1",
    "func_b <- function() 2",
    "func_c <- function() 3"
  ), file.path(tmp_dir, "R", "funcs.R"))

  # Create DESCRIPTION
  writeLines(c(
    "Package: envpkg",
    "Version: 1.0.0",
    "Title: Env Package"
  ), file.path(tmp_dir, "DESCRIPTION"))

  workspace <- analyze_workspace(tmp_dir)
  plan <- discover_files(workspace)

  parsed_files <- list()
  for (file_info in plan$files) {
    ast <- parse_file(file_info)
    if (!is.null(ast)) parsed_files[[length(parsed_files) + 1]] <- ast
  }

  symbol_table <- build_symbol_table(parsed_files, workspace)

  # Verify definitions_env is an environment
  expect_true(is.environment(symbol_table$definitions_env))

  # Verify O(1) lookup works
  for (func_name in c("func_a", "func_b", "func_c")) {
    # Environment lookup should return the definition
    def <- symbol_table$definitions_env[[func_name]]
    expect_false(is.null(def))
    expect_equal(def$name, func_name)
    expect_equal(def$kind, "function")
    expect_true(grepl("scip-r r", def$symbol_id))
  }

  # Non-existent symbol should return NULL
  expect_null(symbol_table$definitions_env[["nonexistent"]])
})
