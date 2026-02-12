# Tests for Shiny dynamic dispatch call edge detection
#
# These tests verify that callModule() and moduleServer() patterns are
# correctly detected and produce SCIP relationship edges to their target
# module server functions.

testthat::test_that("callModule with symbol arg creates call edge", {
  code <- '
BiomarkerBoard <- function(input, output, session) {
  # module logic
}

server <- function(input, output, session) {
  callModule(BiomarkerBoard, "biomarker")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  testthat::expect_true(any(grepl("BiomarkerBoard", sapply(edges, function(e) e$target_name))))

  edge <- edges[[1]]
  testthat::expect_equal(edge$is_reference, TRUE)
  testthat::expect_equal(edge$confidence, 0.9)  # Symbol arg
  testthat::expect_equal(edge$dispatch_type, "callModule")
  testthat::expect_true(!is.null(edge$symbol))

  unlink(tmp)
})

testthat::test_that("callModule with string arg creates call edge with higher confidence", {
  code <- '
BiomarkerBoard <- function(input, output, session) { }

server <- function(input, output, session) {
  callModule("BiomarkerBoard", "biomarker")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  edge <- edges[[1]]
  testthat::expect_equal(edge$target_name, "BiomarkerBoard")
  testthat::expect_equal(edge$confidence, 1.0)  # String literal
  testthat::expect_equal(edge$dispatch_type, "callModule")
  testthat::expect_equal(edge$is_reference, TRUE)

  unlink(tmp)
})

testthat::test_that("moduleServer with symbol arg creates call edge", {
  code <- '
BiomarkerBoard <- function(input, output, session) { }

myModule <- function(id) {
  moduleServer(id, BiomarkerBoard)
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  edge <- edges[[1]]
  testthat::expect_equal(edge$target_name, "BiomarkerBoard")
  testthat::expect_equal(edge$confidence, 0.9)  # Symbol arg
  testthat::expect_equal(edge$dispatch_type, "moduleServer")
  testthat::expect_equal(edge$is_reference, TRUE)

  unlink(tmp)
})

testthat::test_that("moduleServer with string arg creates call edge", {
  code <- '
myModule <- function(id) {
  moduleServer(id, "myServerFunc")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  edge <- edges[[1]]
  testthat::expect_equal(edge$target_name, "myServerFunc")
  testthat::expect_equal(edge$confidence, 1.0)  # String literal
  testthat::expect_equal(edge$dispatch_type, "moduleServer")

  unlink(tmp)
})

testthat::test_that("handles non-existent target gracefully", {
  code <- '
server <- function(input, output, session) {
  callModule(NonExistentFunction, "id")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  # Should not crash; returns edges with local symbol ID fallback
  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  # Should still produce an edge (with local symbol ID fallback)
  testthat::expect_true(length(edges) >= 1)
  edge <- edges[[1]]
  testthat::expect_equal(edge$target_name, "NonExistentFunction")
  testthat::expect_true(grepl("NonExistentFunction", edge$symbol))

  unlink(tmp)
})

testthat::test_that("returns empty list for non-Shiny code", {
  code <- '
x <- 1
foo <- function(a) a + 1
bar <- do.call(mean, list(1:10))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_equal(length(edges), 0)

  unlink(tmp)
})

testthat::test_that("detects multiple dispatch calls in one file", {
  code <- '
ModuleA <- function(input, output, session) { }
ModuleB <- function(input, output, session) { }

server <- function(input, output, session) {
  callModule(ModuleA, "mod_a")
  callModule(ModuleB, "mod_b")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  testthat::expect_equal(length(edges), 2)
  target_names <- sapply(edges, function(e) e$target_name)
  testthat::expect_true("ModuleA" %in% target_names)
  testthat::expect_true("ModuleB" %in% target_names)

  unlink(tmp)
})

testthat::test_that("cross-file resolution uses definitions_env when available", {
  code <- '
server <- function(input, output, session) {
  callModule(ExternalModule, "ext")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  # Create a mock definitions_env with the target symbol
  definitions_env <- new.env(hash = TRUE, parent = emptyenv())
  definitions_env[["ExternalModule"]] <- list(
    name = "ExternalModule",
    file = "R/modules.R",
    kind = "function",
    symbol_id = "scip-r r mypackage 1.0.0 ExternalModule()."
  )

  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    definitions_env,
    basename(tmp)
  )

  testthat::expect_true(length(edges) >= 1)
  edge <- edges[[1]]
  # Should resolve to the global symbol ID from definitions_env
  testthat::expect_equal(edge$symbol, "scip-r r mypackage 1.0.0 ExternalModule().")
  testthat::expect_equal(edge$target_name, "ExternalModule")

  unlink(tmp)
})

testthat::test_that("resolve_shiny_dispatch_target returns NULL for empty name", {
  result <- resolve_shiny_dispatch_target("", NULL, "test.R")
  testthat::expect_null(result)

  result2 <- resolve_shiny_dispatch_target(NULL, NULL, "test.R")
  testthat::expect_null(result2)
})

testthat::test_that("resolve_shiny_dispatch_target falls back to local symbol ID", {
  result <- resolve_shiny_dispatch_target("MyModule", NULL, "R/app.R")
  testthat::expect_true(!is.null(result))
  testthat::expect_true(grepl("local", result))
  testthat::expect_true(grepl("MyModule", result))
})

testthat::test_that("callModule with inline function arg is handled gracefully", {
  # Edge case: callModule with inline anonymous function (no target name)
  code <- '
server <- function(input, output, session) {
  callModule(function(input, output, session) { }, "anon")
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))

  # Should not crash; inline function has no SYMBOL child, so no edge
  edges <- extract_shiny_dispatch_edges(
    ast$parse_data,
    NULL,
    NULL,
    basename(tmp)
  )

  # An inline function won't have a SYMBOL or STR_CONST as target name,
  # so no edge should be produced (or possibly the function token appears)
  # The important thing is no crash
  testthat::expect_true(is.list(edges))

  unlink(tmp)
})
