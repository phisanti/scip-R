oo_regression_project <- function() {
  tmp_dir <- tempfile("scip_oo_index_")
  r_dir <- file.path(tmp_dir, "R")
  dir.create(r_dir, recursive = TRUE)

  r6_path <- file.path(r_dir, "r6_fixture.R")
  writeLines(c(
    "BaseThing <- R6::R6Class(\"BaseThing\",",
    "  public = list(",
    "    initialize = function() NULL",
    "  )",
    ")",
    "",
    "Person <- R6::R6Class(\"Person\",",
    "  inherit = BaseThing,",
    "  public = list(",
    "    initialize = function(name) { self$name <- name; private$.touch() },",
    "    name = NULL,",
    "    greet = function() self$name,",
    "    label = function() self$summary",
    "  ),",
    "  private = list(",
    "    .touch = function() private$count <- private$count + 1,",
    "    count = 0",
    "  ),",
    "  active = list(",
    "    summary = function(value) self$name",
    "  )",
    ")",
    "",
    "person <- Person$new(\"Ada\")"
  ), r6_path)

  s7_path <- file.path(r_dir, "s7_fixture.R")
  writeLines(c(
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
    "recall_store_group <- S7::new_generic(\"recall_store_group\", \"memory\")",
    "",
    "S7::method(recall_store_group, Agent) <- function(memory) {",
    "  S7::prop(memory, \"name\")",
    "  S7::set_props(memory, count = 2)",
    "  memory@name",
    "}",
    "",
    "S7::S7_inherits(memory, Agent)"
  ), s7_path)

  list(root = tmp_dir, cleanup = function() unlink(tmp_dir, recursive = TRUE))
}

expect_symbol_kind <- function(symbols, symbol_id, kind) {
  testthat::expect_true(symbol_id %in% names(symbols), info = symbol_id)
  testthat::expect_equal(symbols[[symbol_id]]$kind, scipr_symbol_kind(kind))
}

testthat::test_that("R6 and S7 fixtures emit class-centric SCIP index data", {
  fixture <- oo_regression_project()
  on.exit(fixture$cleanup(), add = TRUE)

  output <- file.path(fixture$root, "index.scip")
  index_project(list(
    root = fixture$root,
    output = output,
    include = "R/**/*.R",
    namespace = NULL,
    `project-name` = NULL,
    cache = FALSE,
    parallel = 0L
  ))

  index <- scipr_read_index(output)
  r6_doc <- scipr_document(index, "R/r6_fixture.R")
  s7_doc <- scipr_document(index, "R/s7_fixture.R")
  r6_symbols <- scipr_symbols_by_id(r6_doc)
  s7_symbols <- scipr_symbols_by_id(s7_doc)

  r6_class <- build_r6_symbol_id("Person", "R/r6_fixture.R", NULL)
  r6_base <- build_r6_symbol_id("BaseThing", "R/r6_fixture.R", NULL)
  r6_constructor <- paste0(r6_class, "#initialize().")
  r6_method <- paste0(r6_class, "#greet().")
  r6_private_method <- paste0(r6_class, "#`.touch`().")
  r6_field <- paste0(r6_class, "#name.")
  r6_private_field <- paste0(r6_class, "#count.")
  r6_active <- paste0(r6_class, "#summary.")

  expect_symbol_kind(r6_symbols, r6_class, "Class")
  expect_symbol_kind(r6_symbols, r6_base, "Class")
  expect_symbol_kind(r6_symbols, r6_constructor, "Constructor")
  expect_symbol_kind(r6_symbols, r6_method, "Method")
  expect_symbol_kind(r6_symbols, r6_private_method, "Method")
  expect_symbol_kind(r6_symbols, r6_field, "Field")
  expect_symbol_kind(r6_symbols, r6_private_field, "Field")
  expect_symbol_kind(r6_symbols, r6_active, "Property")
  testthat::expect_equal(r6_symbols[[r6_method]]$display_name, "Person$greet")
  testthat::expect_equal(r6_symbols[[r6_active]]$display_name, "Person$summary")

  for (symbol in c(
    r6_class, r6_constructor, r6_method, r6_private_method,
    r6_field, r6_private_field, r6_active
  )) {
    testthat::expect_gt(length(scipr_occurrences_for(r6_doc, symbol, "definition")), 0)
  }
  for (symbol in c(r6_class, r6_constructor, r6_private_method, r6_field, r6_private_field, r6_active)) {
    testthat::expect_gt(length(scipr_occurrences_for(r6_doc, symbol, "reference")), 0)
  }

  s7_class <- build_s7_symbol_id("Agent", "R/s7_fixture.R", NULL)
  s7_generic <- build_s7_symbol_id("recall_store_group", "R/s7_fixture.R", NULL)
  s7_method <- build_s7_symbol_id("recall_store_group,Agent", "R/s7_fixture.R", NULL)
  s7_constructor <- paste0(s7_class, "#initialize().")
  s7_name <- paste0(s7_class, "#name.")
  s7_count <- paste0(s7_class, "#count.")
  s7_parent <- paste0(s7_class, "#parent.")

  expect_symbol_kind(s7_symbols, s7_class, "Class")
  expect_symbol_kind(s7_symbols, s7_generic, "Function")
  expect_symbol_kind(s7_symbols, s7_method, "Method")
  expect_symbol_kind(s7_symbols, s7_constructor, "Constructor")
  expect_symbol_kind(s7_symbols, s7_name, "Property")
  expect_symbol_kind(s7_symbols, s7_count, "Property")
  expect_symbol_kind(s7_symbols, s7_parent, "Property")
  testthat::expect_equal(s7_symbols[[s7_method]]$display_name, "recall_store_group,Agent")

  for (symbol in c(s7_class, s7_generic, s7_method, s7_constructor, s7_name, s7_count, s7_parent)) {
    testthat::expect_gt(length(scipr_occurrences_for(s7_doc, symbol, "definition")), 0)
  }
  for (symbol in c(s7_class, s7_generic, s7_name, s7_count)) {
    testthat::expect_gt(length(scipr_occurrences_for(s7_doc, symbol, "reference")), 0)
  }
})
