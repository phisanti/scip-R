# Tests for plumber REST route handler detection (symbols-plumber.R)
#
# FEATURE: index anonymous plumber route handlers as first-class SCIP symbols.
# Plumber handlers are bare `function(...)` expressions preceded by a `#*`
# annotation block, with no name binding — so the main resolver emits nothing
# for them and the entire REST API surface was dark to semquery.
#
# These tests pin the behavior that protects against regressions:
#   - non-plumber files cost nothing and yield nothing (gate)
#   - route handlers are detected and named from method + path
#   - a definition occurrence is emitted (anonymous handlers are navigable)
#   - nested helper closures in a handler body are NOT mistaken for routes
#   - blocks without a method annotation are skipped (no nameless symbols)

parse_code <- function(code) {
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  on.exit(unlink(tmp))
  parse_file(list(path = tmp, relative_path = basename(tmp)))
}

testthat::test_that("plumber_present gate is FALSE for non-plumber code", {
  ast <- parse_code("x <- 1\n#' roxygen line\nfoo <- function(a) a + 1")
  testthat::expect_false(plumber_present(ast$parse_data))
  testthat::expect_equal(length(extract_plumber_symbols(ast)), 0)
})

testthat::test_that("plumber_present gate is TRUE when a #* comment exists", {
  ast <- parse_code("#* @get /foo\nfunction() { 1 }")
  testthat::expect_true(plumber_present(ast$parse_data))
})

testthat::test_that("synthesize_plumber_name handles paths, params and root", {
  testthat::expect_equal(synthesize_plumber_name("GET", "/dataset/<id>"), "GET_dataset_id")
  testthat::expect_equal(synthesize_plumber_name("POST", "/scatter3d"), "POST_scatter3d")
  testthat::expect_equal(synthesize_plumber_name("GET", "/"), "GET_root")
})

testthat::test_that("a canonical annotated handler becomes a named symbol", {
  code <- paste(
    "#* Get gene metadata for a dataset",
    "#* @tag data",
    "#* @get /dataset/<id>",
    "#* @serializer unboxedJSON",
    "function(id, res) {",
    "  list(id = id)",
    "}",
    sep = "\n"
  )
  ast <- parse_code(code)
  infos <- extract_plumber_symbols(ast)

  testthat::expect_equal(length(infos), 1)
  testthat::expect_equal(infos[[1]]$name, "GET_dataset_id")
  testthat::expect_equal(infos[[1]]$method, "GET")
  # The route line is the first documentation line, full block follows.
  testthat::expect_equal(infos[[1]]$doc[1], "GET /dataset/<id>")
  testthat::expect_true(any(grepl("@serializer unboxedJSON", infos[[1]]$doc)))
})

testthat::test_that("build emits BOTH a symbol entry and a definition occurrence", {
  # Anonymous handlers have no main-pass occurrence; the builder must supply
  # one or the symbol would be un-navigable (the core point of the feature).
  code <- "#* @get /health\nfunction() { list(status = \"ok\") }"
  ast <- parse_code(code)
  entries <- build_plumber_symbol_entries(ast, "svc.R", NULL)

  testthat::expect_equal(length(entries$symbols), 1)
  testthat::expect_equal(length(entries$occurrences), 1)

  sym <- entries$symbols[[1]]
  occ <- entries$occurrences[[1]]
  testthat::expect_equal(sym$display_name, "GET_health")
  testthat::expect_equal(sym$kind, "plumber_endpoint")
  testthat::expect_equal(sym$symbol, occ$symbol)
  testthat::expect_equal(occ$role, "definition")
  # Definition range points at the `function` keyword (0-based line).
  testthat::expect_equal(occ$range[1], 1)
  # Enclosing range spans the full handler for go-to-definition context.
  testthat::expect_false(is.null(occ$closure_func_range))
})

testthat::test_that("nested helper closures in a handler body are not routes", {
  # tryCatch's error = function(e) ... is a nested closure with NO #* block;
  # it must never be mistaken for a second route.
  code <- paste(
    "#* @get /<dataset_id>",
    "#* @serializer unboxedJSON",
    "function(dataset_id, res) {",
    "  tryCatch(compute(dataset_id),",
    "    error = function(e) api_error(e, res))",
    "}",
    sep = "\n"
  )
  ast <- parse_code(code)
  infos <- extract_plumber_symbols(ast)

  testthat::expect_equal(length(infos), 1)
  testthat::expect_equal(infos[[1]]$name, "GET_dataset_id")
})

testthat::test_that("multiple routes in one file each get a symbol", {
  code <- paste(
    "#* @get /a",
    "function() { 1 }",
    "",
    "#* @post /b/<x>",
    "function(x) { x }",
    sep = "\n"
  )
  ast <- parse_code(code)
  entries <- build_plumber_symbol_entries(ast, "svc.R", NULL)
  names <- vapply(entries$symbols, function(s) s$display_name, character(1))

  testthat::expect_setequal(names, c("GET_a", "POST_b_x"))
})

testthat::test_that("a #* block with no method annotation is skipped", {
  # @filter-only / description-only blocks are not routes in this pass.
  code <- "#* @filter cors\nfunction(req, res) { plumber::forward() }"
  ast <- parse_code(code)
  testthat::expect_equal(length(extract_plumber_symbols(ast)), 0)
})

testthat::test_that("a named-assignment handler is not double-indexed", {
  # `h <- function()` after a #* block already gets a symbol from the main
  # resolver; the plumber extractor must skip it to avoid a duplicate symbol.
  ast <- parse_code("#* @get /foo\nh <- function(req, res) { 1 }")
  testthat::expect_equal(length(extract_plumber_symbols(ast)), 0)
})

testthat::test_that("a method annotation with no path is skipped", {
  # The route regex requires a path token after the method; a bare `@get`
  # is malformed and must not synthesize a nameless symbol.
  ast <- parse_code("#* @get\nfunction() { 1 }")
  testthat::expect_equal(length(extract_plumber_symbols(ast)), 0)
})

testthat::test_that("namespaced symbol id uses the package-export format", {
  ast <- parse_code("#* @get /health\nfunction() { list(ok = TRUE) }")
  entries <- build_plumber_symbol_entries(ast, "svc.R", namespace = "mypkg")
  testthat::expect_equal(length(entries$symbols), 1)
  # namespaced ids are "<ns> <path>#<name>", not the "local ..." local form.
  testthat::expect_match(entries$symbols[[1]]$symbol, "^mypkg svc.R#GET_health$")
})

testthat::test_that("a dangling #* block far from any function is skipped", {
  code <- paste(
    "#* @get /orphan",
    "",
    "",
    "",
    "x <- 1",
    "named <- function() 2",
    sep = "\n"
  )
  ast <- parse_code(code)
  testthat::expect_equal(length(extract_plumber_symbols(ast)), 0)
})
