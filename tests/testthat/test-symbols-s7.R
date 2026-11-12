# Tests for S7 object system symbol detection

s7_fixture_ast <- function() {
  tmp_dir <- tempfile("scip_s7_")
  dir.create(tmp_dir)
  file_path <- file.path(tmp_dir, "s7_fixture.R")
  writeLines(c(
    "#' Agent class docs",
    "Agent <- S7::new_class(\"Agent\",",
    "  properties = list(",
    "    name = class_character,",
    "    count = S7::new_property(class_integer),",
    "    parent = S7::new_property(Agent)",
    "  ),",
    "  constructor = function(name) new_object(Agent, name = name),",
    "  validator = function(self) NULL",
    ")",
    "",
    "#' Store group docs",
    "recall_store_group <- S7::new_generic(\"recall_store_group\", \"memory\")",
    "",
    "S7::method(recall_store_group, Agent) <- function(memory) {",
    "  S7::prop(memory, \"name\")",
    "  S7::set_props(memory, count = 2)",
    "  memory@name",
    "}",
    "",
    "S7::S7_inherits(memory, Agent)"
  ), file_path)

  list(
    ast = parse_file(list(path = file_path, relative_path = "s7_fixture.R")),
    cleanup = function() unlink(tmp_dir, recursive = TRUE)
  )
}

testthat::test_that("extract_s7_symbols returns empty for non-S7 code", {
  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c("x <- 1", "foo <- function(a) a + 1"), tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  result <- extract_s7_symbols(ast)

  testthat::expect_equal(length(result$definitions), 0)
  testthat::expect_equal(length(result$references), 0)
})

testthat::test_that("extract_s7_symbols detects classes, generics, methods, and references", {
  fixture <- s7_fixture_ast()
  on.exit(fixture$cleanup(), add = TRUE)

  result <- extract_s7_symbols(fixture$ast)
  types <- vapply(result$definitions, function(x) x$type, character(1))

  testthat::expect_equal(sum(types == "s7_class"), 1)
  testthat::expect_equal(sum(types == "s7_generic"), 1)
  testthat::expect_equal(sum(types == "s7_method"), 1)

  class_info <- result$definitions[[which(types == "s7_class")]]
  testthat::expect_equal(class_info$class_name, "Agent")
  testthat::expect_true(class_info$has_constructor)
  testthat::expect_setequal(
    vapply(class_info$properties, function(x) x$name, character(1)),
    c("name", "count", "parent")
  )

  generic_info <- result$definitions[[which(types == "s7_generic")]]
  testthat::expect_equal(generic_info$generic_name, "recall_store_group")
  testthat::expect_equal(generic_info$dispatch_arg, "memory")

  method_info <- result$definitions[[which(types == "s7_method")]]
  testthat::expect_equal(method_info$generic_name, "recall_store_group")
  testthat::expect_equal(method_info$class_name, "Agent")
  testthat::expect_setequal(
    vapply(method_info$property_refs, function(x) x$name, character(1)),
    c("name", "count")
  )
  testthat::expect_equal(length(result$references), 1)
  testthat::expect_equal(result$references[[1]]$class_name, "Agent")
})

testthat::test_that("build_s7_symbol_entries emits symbol information and occurrences", {
  fixture <- s7_fixture_ast()
  on.exit(fixture$cleanup(), add = TRUE)

  entries <- build_s7_symbol_entries(fixture$ast, "s7_fixture.R", NULL)
  kinds <- vapply(entries$symbols, function(x) x$kind, character(1))
  names <- vapply(entries$symbols, function(x) x$display_name, character(1))

  testthat::expect_true("s7_class" %in% kinds)
  testthat::expect_true("s7_property" %in% kinds)
  testthat::expect_true("s7_constructor" %in% kinds)
  testthat::expect_true("s7_generic" %in% kinds)
  testthat::expect_true("s7_method" %in% kinds)
  testthat::expect_true("Agent" %in% names)
  testthat::expect_true("recall_store_group" %in% names)
  testthat::expect_true("recall_store_group,Agent" %in% names)

  class_symbol <- build_s7_symbol_id("Agent", "s7_fixture.R", NULL)
  generic_symbol <- build_s7_symbol_id("recall_store_group", "s7_fixture.R", NULL)
  method_symbol <- build_s7_symbol_id("recall_store_group,Agent", "s7_fixture.R", NULL)
  property_symbol <- paste0(class_symbol, "#name.")

  definitions <- Filter(function(x) identical(x$role, "definition"), entries$occurrences)
  references <- Filter(function(x) identical(x$role, "reference"), entries$occurrences)

  testthat::expect_true(any(vapply(definitions, function(x) identical(x$symbol, class_symbol), logical(1))))
  testthat::expect_true(any(vapply(definitions, function(x) identical(x$symbol, generic_symbol), logical(1))))
  testthat::expect_true(any(vapply(definitions, function(x) identical(x$symbol, method_symbol), logical(1))))
  testthat::expect_true(any(vapply(references, function(x) identical(x$symbol, generic_symbol), logical(1))))
  testthat::expect_true(any(vapply(references, function(x) identical(x$symbol, class_symbol), logical(1))))
  testthat::expect_true(any(vapply(references, function(x) identical(x$symbol, property_symbol), logical(1))))
  class_references <- Filter(function(x) identical(x$symbol, class_symbol), references)
  testthat::expect_gte(length(class_references), 3)
})

testthat::test_that("S7 symbols integrate into resolve_symbols", {
  fixture <- s7_fixture_ast()
  on.exit(fixture$cleanup(), add = TRUE)

  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(fixture$ast, workspace, track_externals = FALSE)

  s7_symbols <- Filter(function(s) grepl("^s7_", s$kind), result$symbols)
  s7_occurrences <- Filter(function(o) grepl("#Agent|#recall_store_group", o$symbol), result$occurrences)

  testthat::expect_gte(length(s7_symbols), 5)
  testthat::expect_true(any(vapply(s7_occurrences, function(o) identical(o$role, "definition"), logical(1))))
  testthat::expect_true(any(vapply(s7_occurrences, function(o) identical(o$role, "reference"), logical(1))))
})
