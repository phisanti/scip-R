# Tests for Shiny symbol detection module
# BUG FIX: shiny_reactive kind serialized as Variable instead of Function
# reactive()/reactiveVal()/reactiveValues() were emitted as SCIP kind 61 (Variable).
# Navigation tools commonly hide Variable symbols by default, which made every
# Shiny reactive expression invisible to symbol search.
# Fix: serializer.R now emits shiny_reactive as kind 17 (Function), since reactives
# are callable objects (x() retrieves the value).
# Anti-regression tests below:
#   - test_shiny_reactive_kind_is_function
#   - test_reactiveVal_kind_is_function
#   - test_reactiveValues_kind_is_function

testthat::test_that("extract_shiny_symbols returns empty for non-Shiny code", {
  code <- "x <- 1\nfoo <- function(a) a + 1"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)
  testthat::expect_equal(length(result), 0)
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects moduleServer", {
  code <- "myModule <- function(id) {
  moduleServer(id, function(input, output, session) {
    # module logic
  })
}"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  module_symbols <- Filter(function(x) x$type == "shiny_module", result)
  testthat::expect_true(length(module_symbols) >= 1)
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects reactiveVal", {
  code <- "counter <- reactiveVal(0)"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  reactive_symbols <- Filter(function(x) x$type == "shiny_reactive", result)
  testthat::expect_equal(length(reactive_symbols), 1)
  testthat::expect_equal(reactive_symbols[[1]]$reactive_type, "reactiveVal")
  testthat::expect_equal(reactive_symbols[[1]]$assigned_name, "counter")
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects reactiveValues", {
  code <- 'values <- reactiveValues(count = 0, name = "")'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  reactive_symbols <- Filter(function(x) x$type == "shiny_reactive", result)
  testthat::expect_equal(length(reactive_symbols), 1)
  testthat::expect_equal(reactive_symbols[[1]]$reactive_type, "reactiveValues")
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects observe", {
  code <- "observe({ print(input$x) })"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  observer_symbols <- Filter(function(x) x$type == "shiny_observer", result)
  testthat::expect_equal(length(observer_symbols), 1)
  testthat::expect_equal(observer_symbols[[1]]$observer_type, "observe")
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects observeEvent", {
  code <- 'observeEvent(input$button, { print("clicked") })'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  observer_symbols <- Filter(function(x) x$type == "shiny_observer", result)
  testthat::expect_equal(length(observer_symbols), 1)
  testthat::expect_equal(observer_symbols[[1]]$observer_type, "observeEvent")
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects renderPlot", {
  code <- "renderPlot({ plot(1:10) })"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  output_symbols <- Filter(function(x) x$type == "shiny_output", result)
  testthat::expect_equal(length(output_symbols), 1)
  testthat::expect_equal(output_symbols[[1]]$render_type, "renderPlot")
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects module function patterns", {
  code <- '
counterUI <- function(id) {
  ns <- NS(id)
  tagList(numericInput(ns("n"), "N", 0))
}

counterServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive(input$n * 2)
  })
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  module_funcs <- Filter(function(x) x$type == "shiny_module_function", result)
  testthat::expect_equal(length(module_funcs), 2)

  func_names <- sapply(module_funcs, function(x) x$func_name)
  testthat::expect_true("counterUI" %in% func_names)
  testthat::expect_true("counterServer" %in% func_names)
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols detects NS calls", {
  code <- 'ns <- NS("mymodule")'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  ns_symbols <- Filter(function(x) x$type == "shiny_namespace", result)
  testthat::expect_equal(length(ns_symbols), 1)
  testthat::expect_equal(ns_symbols[[1]]$ns_id, "mymodule")
  unlink(tmp)
})

testthat::test_that("build_shiny_symbol_entries creates proper entries", {
  code <- "myReactive <- reactiveVal(0)"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  entries <- build_shiny_symbol_entries(ast, basename(tmp), NULL)

  testthat::expect_true(length(entries) >= 1)
  reactive_entry <- entries[[1]]
  testthat::expect_equal(reactive_entry$kind, "shiny_reactive")
  testthat::expect_equal(reactive_entry$display_name, "myReactive")
  unlink(tmp)
})

testthat::test_that("Shiny symbols integrate into resolve_symbols", {
  code <- "data <- reactiveValues(x = 1, y = 2)"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())

  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  shiny_symbols <- Filter(function(s) grepl("^shiny_", s$kind), result$symbols)
  testthat::expect_true(length(shiny_symbols) >= 1)
  unlink(tmp)
})

testthat::test_that("extract_shiny_symbols handles complex Shiny app", {
  code <- '
library(shiny)

ui <- fluidPage(
  numericInput("n", "N", 10),
  plotOutput("hist")
)

server <- function(input, output, session) {
  data <- reactive({
    rnorm(input$n)
  })

  output$hist <- renderPlot({
    hist(data())
  })

  observe({
    print(paste("n =", input$n))
  })
}
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_shiny_symbols(ast)

  testthat::expect_true(length(result) >= 3)

  types <- sapply(result, function(x) x$type)
  testthat::expect_true("shiny_reactive" %in% types)
  testthat::expect_true("shiny_output" %in% types)
  testthat::expect_true("shiny_observer" %in% types)
  unlink(tmp)
})

# Anti-regression: reactive() serialized as Function kind (not Variable)
# Before fix: shiny_reactive -> Variable (61), hidden by Variable filters
# After fix:  shiny_reactive -> Function (17)
testthat::test_that("test_shiny_reactive_kind_is_function", {
  code <- "server <- function(input, output, session) {\n  my_data <- reactive({ input$n * 2 })\n}"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  reactive_sym <- Filter(
    function(s) !is.null(s$display_name) && s$display_name == "my_data" && s$kind == "shiny_reactive",
    result$symbols
  )
  testthat::expect_true(length(reactive_sym) >= 1, info = "reactive() assignment must be indexed")
  testthat::expect_equal(reactive_sym[[1]]$kind, "shiny_reactive")

  # Verify SCIP serialization emits Function kind (17), not Variable (61)
  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  sym_info <- build_symbol_information_message(reactive_sym[[1]])
  testthat::expect_equal(sym_info$kind, as.integer(kind_enum$Function),
    info = "shiny_reactive must serialize to Function (17), not Variable (61)")
  unlink(tmp)
})

testthat::test_that("test_reactiveVal_kind_is_function", {
  code <- "server <- function(input, output, session) {\n  counter <- reactiveVal(0)\n}"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  reactive_sym <- Filter(
    function(s) !is.null(s$display_name) && s$display_name == "counter" && s$kind == "shiny_reactive",
    result$symbols
  )
  testthat::expect_true(length(reactive_sym) >= 1, info = "reactiveVal() assignment must be indexed")
  testthat::expect_equal(reactive_sym[[1]]$kind, "shiny_reactive")

  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  sym_info <- build_symbol_information_message(reactive_sym[[1]])
  testthat::expect_equal(sym_info$kind, as.integer(kind_enum$Function),
    info = "reactiveVal must serialize to Function (17), not Variable (61)")
  unlink(tmp)
})

testthat::test_that("test_reactiveValues_kind_is_function", {
  code <- "server <- function(input, output, session) {\n  state <- reactiveValues(x = 1, y = 2)\n}"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)

  reactive_sym <- Filter(
    function(s) !is.null(s$display_name) && s$display_name == "state" && s$kind == "shiny_reactive",
    result$symbols
  )
  testthat::expect_true(length(reactive_sym) >= 1, info = "reactiveValues() assignment must be indexed")
  testthat::expect_equal(reactive_sym[[1]]$kind, "shiny_reactive")

  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  sym_info <- build_symbol_information_message(reactive_sym[[1]])
  testthat::expect_equal(sym_info$kind, as.integer(kind_enum$Function),
    info = "reactiveValues must serialize to Function (17), not Variable (61)")
  unlink(tmp)
})
