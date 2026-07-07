# Tests for R6 object system symbol detection
# This validates the symbols-r6.R module implementation

# ============================================================================
# Level 1: Unit Tests - Pattern Recognition
# ============================================================================

test_that("detects basic R6Class definition with R6:: prefix", {
  tmp_dir <- tempfile("scip_r6_basic_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "basic.R")
  writeLines(c(
    "Person <- R6::R6Class('Person',",
    "  public = list(",
    "    name = NULL,",
    "    initialize = function(name) {",
    "      self$name <- name",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "basic.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should detect one R6 class
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$type, "r6_class")
  expect_equal(r6_symbols[[1]]$class_name, "Person")
})

test_that("detects R6Class without namespace prefix", {
  tmp_dir <- tempfile("scip_r6_no_ns_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "no_namespace.R")
  writeLines(c(
    "Counter <- R6Class('Counter',",
    "  public = list(",
    "    count = 0,",
    "    increment = function() self$count <- self$count + 1",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "no_namespace.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should detect R6Class even without R6:: prefix
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "Counter")
})

test_that("ignores regular functions not related to R6", {
  tmp_dir <- tempfile("scip_r6_regular_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "regular.R")
  writeLines(c(
    "regular_function <- function(x) { x + 1 }",
    "MyClass <- function(x) structure(x, class = 'MyClass')"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "regular.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should find zero R6 classes
  expect_equal(length(r6_symbols), 0)
})

test_that("ignores S3 class constructors", {
  tmp_dir <- tempfile("scip_r6_s3_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "s3.R")
  writeLines(c(
    "new_person <- function(name) {",
    "  structure(list(name = name), class = 'person')",
    "}"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "s3.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should not detect S3 as R6
  expect_equal(length(r6_symbols), 0)
})

# ============================================================================
# Level 2: True Positives vs True Negatives
# ============================================================================

test_that("R6 detection true positives", {
  tmp_dir <- tempfile("scip_r6_positive_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "r6_positives.R")
  writeLines(c(
    "# TRUE POSITIVE 1: Full R6::R6Class syntax",
    "Counter <- R6::R6Class('Counter',",
    "  public = list(",
    "    count = 0,",
    "    increment = function() self$count <- self$count + 1",
    "  )",
    ")",
    "",
    "# TRUE POSITIVE 2: Shorthand R6Class (after library(R6))",
    "Point <- R6Class('Point',",
    "  public = list(x = 0, y = 0)",
    ")",
    "",
    "# TRUE POSITIVE 3: R6 with private members",
    "BankAccount <- R6Class('BankAccount',",
    "  private = list(balance = 0),",
    "  public = list(",
    "    deposit = function(amt) private$balance <- private$balance + amt",
    "  )",
    ")",
    "",
    "# TRUE POSITIVE 4: R6 with inheritance",
    "Manager <- R6Class('Manager',",
    "  inherit = Person,",
    "  public = list(team_size = 0)",
    ")",
    "",
    "# TRUE POSITIVE 5: R6 with active bindings",
    "Temperature <- R6Class('Temperature',",
    "  private = list(celsius = 0),",
    "  active = list(",
    "    fahrenheit = function(value) {",
    "      if (missing(value)) return(private$celsius * 9/5 + 32)",
    "      private$celsius <- (value - 32) * 5/9",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "r6_positives.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should detect all 5 R6 classes
  expect_equal(length(r6_symbols), 5)
  expect_true(all(sapply(r6_symbols, function(x) x$type == "r6_class")))
  
  # Verify names
  class_names <- sapply(r6_symbols, function(x) x$class_name)
  expect_setequal(class_names, c("Counter", "Point", "BankAccount", "Manager", "Temperature"))
  
  # Verify inheritance detection
  manager_class <- Find(function(x) x$class_name == "Manager", r6_symbols)
  expect_true(!is.null(manager_class$inherits_from))
})

test_that("R6 detection true negatives", {
  tmp_dir <- tempfile("scip_r6_negative_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "r6_negatives.R")
  writeLines(c(
    "# TRUE NEGATIVE 1: Regular function with 'class' in name",
    "MyClass <- function(x) structure(x, class = 'MyClass')",
    "",
    "# TRUE NEGATIVE 2: S3 class constructor",
    "new_person <- function(name) {",
    "  structure(list(name = name), class = 'person')",
    "}",
    "",
    "# TRUE NEGATIVE 3: S4 class definition",
    "setClass('Person', slots = c(name = 'character'))",
    "",
    "# TRUE NEGATIVE 4: Variable named R6Class (shadowing)",
    "R6Class <- list(name = 'not a class')",
    "",
    "# TRUE NEGATIVE 5: String containing R6Class",
    "help_text <- 'Use R6Class to create classes'",
    "",
    "# TRUE NEGATIVE 6: Comment mentioning R6",
    "# This uses R6Class internally",
    "regular_function <- function() { }",
    "",
    "# TRUE NEGATIVE 7: Namespace call without proper structure",
    "obj <- R6::validate(something)"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "r6_negatives.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should detect zero R6 classes
  expect_equal(length(r6_symbols), 0)
})

# ============================================================================
# Level 3: Real-World Package Patterns
# ============================================================================

test_that("detects R6 patterns from plumber package", {
  tmp_dir <- tempfile("scip_r6_plumber_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "plumber.R")
  writeLines(c(
    "Plumber <- R6Class('Plumber',",
    "  public = list(",
    "    endpoints = list(),",
    "    handle = function(req, res) {",
    "      # routing logic",
    "    },",
    "    run = function(host = '127.0.0.1', port = 8000) {",
    "      # server startup",
    "    }",
    "  ),",
    "  private = list(",
    "    routes = list()",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "plumber.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "Plumber")
  
  # Verify it detected public members
  if (!is.null(r6_symbols[[1]]$public_members)) {
    expect_gte(length(r6_symbols[[1]]$public_members), 1)
  }
})

test_that("detects R6 patterns from reticulate package", {
  tmp_dir <- tempfile("scip_r6_reticulate_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "reticulate.R")
  writeLines(c(
    "PythonEnvironment <- R6Class('PythonEnvironment',",
    "  public = list(",
    "    initialize = function(python_path) {",
    "      private$python <- python_path",
    "    }",
    "  ),",
    "  private = list(",
    "    python = NULL",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "reticulate.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "PythonEnvironment")
})

test_that("detects R6 in shiny app state management", {
  tmp_dir <- tempfile("scip_r6_shiny_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "shiny_state.R")
  writeLines(c(
    "AppState <- R6Class('AppState',",
    "  public = list(",
    "    data = NULL,",
    "    filters = list(),",
    "    update_data = function(new_data) {",
    "      self$data <- new_data",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "shiny_state.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "AppState")
})

# ============================================================================
# Level 4: Edge Cases & Ambiguity Resolution
# ============================================================================

test_that("handles R6 classes with complex member definitions", {
  tmp_dir <- tempfile("scip_r6_complex_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "complex.R")
  writeLines(c(
    "ComplexClass <- R6Class('ComplexClass',",
    "  public = list(",
    "    # Field with complex default value",
    "    data = data.frame(x = 1:10, y = rnorm(10)),",
    "    ",
    "    # Method with multiple parameters",
    "    process = function(x, y, z = NULL, ...) {",
    "      if (is.null(z)) z <- private$default_z",
    "      x + y + z",
    "    },",
    "    ",
    "    # Method returning self for chaining",
    "    set_data = function(d) {",
    "      self$data <- d",
    "      invisible(self)",
    "    }",
    "  ),",
    "  private = list(",
    "    default_z = 5,",
    "    ",
    "    # Private method",
    "    validate = function() {",
    "      stopifnot(is.data.frame(self$data))",
    "    }",
    "  ),",
    "  active = list(",
    "    # Active binding with getter/setter",
    "    mean_value = function(value) {",
    "      if (missing(value)) return(mean(self$data$y))",
    "      self$data$y <- rep(value, nrow(self$data))",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "complex.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  
  cls <- r6_symbols[[1]]
  expect_equal(cls$class_name, "ComplexClass")
  
  # Verify structure detection (if implemented)
  if (!is.null(cls$has_public)) {
    expect_true(cls$has_public)
  }
  if (!is.null(cls$has_private)) {
    expect_true(cls$has_private)
  }
  if (!is.null(cls$has_active)) {
    expect_true(cls$has_active)
  }
})

test_that("distinguishes R6 from R6Class in comments", {
  tmp_dir <- tempfile("scip_r6_comments_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "comments.R")
  writeLines(c(
    "# Use R6Class to define classes",
    "# Example: R6Class('Name', ...)",
    "",
    "regular_function <- function() {",
    "  # This isn't R6Class('Test')",
    "  return(42)",
    "}"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "comments.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Comments should be ignored
  expect_equal(length(r6_symbols), 0)
})

test_that("handles nested R6 definitions", {
  tmp_dir <- tempfile("scip_r6_nested_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "nested.R")
  writeLines(c(
    "# R6 class that creates other R6 classes",
    "ClassFactory <- R6Class('ClassFactory',",
    "  public = list(",
    "    create_class = function(name) {",
    "      R6Class(name,",
    "        public = list(value = 0)",
    "      )",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "nested.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should detect at least ClassFactory
  # Nested R6Class is optional to detect
  expect_gte(length(r6_symbols), 1)
  
  factory_class <- Find(function(x) x$class_name == "ClassFactory", r6_symbols)
  expect_true(!is.null(factory_class))
})

test_that("handles R6Class with multiline string literals", {
  tmp_dir <- tempfile("scip_r6_multiline_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "multiline.R")
  writeLines(c(
    "Documentation <- R6Class('Documentation',",
    "  public = list(",
    "    help_text = 'This is a long",
    "documentation string",
    "that spans multiple lines',",
    "    show = function() cat(self$help_text)",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "multiline.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  # Should still detect the class despite multiline strings
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "Documentation")
})

test_that("handles R6Class with portable = TRUE/FALSE", {
  tmp_dir <- tempfile("scip_r6_portable_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "portable.R")
  writeLines(c(
    "NonPortable <- R6Class('NonPortable',",
    "  portable = FALSE,",
    "  public = list(",
    "    value = 0",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "portable.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "NonPortable")
})

# ============================================================================
# Level 5: Integration Test with SCIP Descriptors
# ============================================================================

test_that("generates correct SCIP descriptors for R6 symbols", {
  skip("SCIP descriptor generation not yet implemented")
  
  tmp_dir <- tempfile("scip_r6_integration_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "package.R")
  writeLines(c(
    "Person <- R6Class('Person',",
    "  public = list(",
    "    name = NULL,",
    "    greet = function() cat('Hello, ', self$name)",
    "  ),",
    "  private = list(",
    "    id = NULL",
    "  )",
    ")"
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
  r6_symbols <- extract_r6_symbols(ast)
  
  # Convert to SCIP descriptors
  scip_symbols <- lapply(r6_symbols, function(sym) {
    r6_to_scip(sym, workspace)
  })
  
  # Expected descriptors:
  # scip-r mypackage 1.0.0 Person#
  # scip-r mypackage 1.0.0 Person#name.
  # scip-r mypackage 1.0.0 Person#greet().
  # scip-r mypackage 1.0.0 Person#id.(private)
  
  descriptor_strings <- sapply(scip_symbols, function(x) x$symbol)
  
  expect_true(any(grepl("Person#$", descriptor_strings)))
  expect_true(any(grepl("Person#greet\\(\\)\\.", descriptor_strings)))
})

test_that("build_r6_symbol_entries emits class-owned definitions and references", {
  tmp_dir <- tempfile("scip_r6_occurrences_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "r6_occurrences.R")
  writeLines(c(
    "Person <- R6::R6Class('Person',",
    "  public = list(",
    "    initialize = function(name) { self$name <- name; private$.touch() },",
    "    name = NULL,",
    "    greet = function() self$name,",
    "    get_label = function() self$label",
    "  ),",
    "  private = list(",
    "    .touch = function() private$count <- private$count + 1,",
    "    count = 0",
    "  ),",
    "  active = list(",
    "    label = function(value) self$name",
    "  )",
    ")",
    "",
    "person <- Person$new('Ada')"
  ), file_path)

  ast <- parse_file(list(path = file_path, relative_path = "r6_occurrences.R"))
  entries <- build_r6_symbol_entries(ast, "r6_occurrences.R", NULL)

  class_symbol <- build_r6_symbol_id("Person", "r6_occurrences.R", NULL)
  constructor_symbol <- paste0(class_symbol, "#initialize().")
  greet_symbol <- paste0(class_symbol, "#greet().")
  name_symbol <- paste0(class_symbol, "#name.")
  touch_symbol <- paste0(class_symbol, "#`.touch`().")
  count_symbol <- paste0(class_symbol, "#count.")
  label_symbol <- paste0(class_symbol, "#label.")

  symbols_by_id <- stats::setNames(entries$symbols, vapply(entries$symbols, `[[`, character(1), "symbol"))
  testthat::expect_equal(symbols_by_id[[class_symbol]]$kind, "r6_class")
  testthat::expect_equal(symbols_by_id[[constructor_symbol]]$kind, "r6_constructor")
  testthat::expect_equal(symbols_by_id[[greet_symbol]]$kind, "r6_method")
  testthat::expect_equal(symbols_by_id[[name_symbol]]$kind, "r6_field")
  testthat::expect_equal(symbols_by_id[[touch_symbol]]$kind, "r6_method")
  testthat::expect_equal(symbols_by_id[[count_symbol]]$kind, "r6_field")
  testthat::expect_equal(symbols_by_id[[label_symbol]]$kind, "r6_active")
  testthat::expect_equal(symbols_by_id[[greet_symbol]]$display_name, "Person$greet")

  definitions <- Filter(function(x) identical(x$role, "definition"), entries$occurrences)
  references <- Filter(function(x) identical(x$role, "reference"), entries$occurrences)

  definition_symbols <- vapply(definitions, `[[`, character(1), "symbol")
  reference_symbols <- vapply(references, `[[`, character(1), "symbol")

  testthat::expect_true(class_symbol %in% definition_symbols)
  testthat::expect_true(constructor_symbol %in% definition_symbols)
  testthat::expect_true(greet_symbol %in% definition_symbols)
  testthat::expect_true(name_symbol %in% definition_symbols)
  testthat::expect_true(touch_symbol %in% definition_symbols)
  testthat::expect_true(count_symbol %in% definition_symbols)
  testthat::expect_true(label_symbol %in% definition_symbols)
  class_definition <- definitions[[which(definition_symbols == class_symbol)[1]]]
  testthat::expect_equal(class_definition$range, c(0, 0, 0, 6))

  testthat::expect_true(class_symbol %in% reference_symbols)
  testthat::expect_true(constructor_symbol %in% reference_symbols)
  testthat::expect_true(name_symbol %in% reference_symbols)
  testthat::expect_true(touch_symbol %in% reference_symbols)
  testthat::expect_true(count_symbol %in% reference_symbols)
  testthat::expect_true(label_symbol %in% reference_symbols)
})

test_that("R6 symbols and occurrences integrate into resolve_symbols", {
  tmp_dir <- tempfile("scip_r6_resolve_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "r6_resolve.R")
  writeLines(c(
    "Widget <- R6::R6Class('Widget',",
    "  public = list(",
    "    initialize = function(value) self$value <- value,",
    "    value = NULL,",
    "    read = function() self$value",
    "  )",
    ")"
  ), file_path)

  ast <- parse_file(list(path = file_path, relative_path = "r6_resolve.R"))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  r6_symbols <- Filter(function(s) grepl("^r6_", s$kind), result$symbols)
  r6_occurrences <- Filter(function(o) grepl("#Widget", o$symbol), result$occurrences)

  testthat::expect_gte(length(r6_symbols), 4)
  testthat::expect_true(any(vapply(r6_occurrences, function(o) identical(o$role, "definition"), logical(1))))
  testthat::expect_true(any(vapply(r6_occurrences, function(o) identical(o$role, "reference"), logical(1))))
})

# ============================================================================
# Level 6: Performance Benchmark
# ============================================================================

test_that("R6 extraction has acceptable performance", {
  skip_on_cran()  # Performance tests can be slow on CRAN
  
  # Generate 50 R6 classes
  classes <- sapply(1:50, function(i) {
    sprintf(
      "Class%d <- R6Class('Class%d',\n  public = list(\n    value = %d,\n    method = function() self$value * 2\n  )\n)",
      i, i, i
    )
  })
  
  tmp_dir <- tempfile("scip_r6_perf_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "large.R")
  writeLines(paste(classes, collapse = "\n\n"), file_path)
  
  file_info <- list(path = file_path, relative_path = "large.R")
  
  timing <- system.time({
    ast <- parse_file(file_info)
    r6_symbols <- extract_r6_symbols(ast)
  })
  
  # Should process 50 classes in < 100ms
  expect_lt(timing["elapsed"], 0.1)
  expect_equal(length(r6_symbols), 50)
})

# ============================================================================
# Level 7: Complex Real-World Example
# ============================================================================

test_that("handles complex R6 from {R6} package examples", {
  tmp_dir <- tempfile("scip_r6_queue_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "queue.R")
  writeLines(c(
    "Queue <- R6Class('Queue',",
    "  public = list(",
    "    initialize = function(...) {",
    "      for (item in list(...)) {",
    "        self$add(item)",
    "      }",
    "    },",
    "    add = function(x) {",
    "      private$queue <- c(private$queue, list(x))",
    "      invisible(self)",
    "    },",
    "    remove = function() {",
    "      if (private$length() == 0) return(NULL)",
    "      head <- private$queue[[1]]",
    "      private$queue <- private$queue[-1]",
    "      head",
    "    }",
    "  ),",
    "  private = list(",
    "    queue = list(),",
    "    length = function() base::length(private$queue)",
    "  )",
    ")",
    "",
    "# Inheritance example",
    "HistoryQueue <- R6Class('HistoryQueue',",
    "  inherit = Queue,",
    "  public = list(",
    "    show = function() {",
    "      cat('Next item is:\\n')",
    "      print(private$queue[[1]])",
    "    }",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "queue.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 2)
  
  # Verify Queue structure
  queue_class <- Find(function(x) x$class_name == "Queue", r6_symbols)
  expect_true(!is.null(queue_class))
  
  # Verify HistoryQueue inherits from Queue
  hist_class <- Find(function(x) x$class_name == "HistoryQueue", r6_symbols)
  expect_true(!is.null(hist_class))
  if (!is.null(hist_class$inherits_from)) {
    expect_equal(hist_class$inherits_from, "Queue")
  }
})

test_that("handles R6 with cloneable = FALSE", {
  tmp_dir <- tempfile("scip_r6_cloneable_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "cloneable.R")
  writeLines(c(
    "Singleton <- R6Class('Singleton',",
    "  cloneable = FALSE,",
    "  public = list(",
    "    value = NULL",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "cloneable.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "Singleton")
})

test_that("handles R6 with lock_objects = TRUE", {
  tmp_dir <- tempfile("scip_r6_lock_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  
  file_path <- file.path(tmp_dir, "lock.R")
  writeLines(c(
    "Immutable <- R6Class('Immutable',",
    "  lock_objects = TRUE,",
    "  public = list(",
    "    value = 0",
    "  )",
    ")"
  ), file_path)
  
  file_info <- list(path = file_path, relative_path = "lock.R")
  ast <- parse_file(file_info)
  r6_symbols <- extract_r6_symbols(ast)
  
  expect_equal(length(r6_symbols), 1)
  expect_equal(r6_symbols[[1]]$class_name, "Immutable")
})
