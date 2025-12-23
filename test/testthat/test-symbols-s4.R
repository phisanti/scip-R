# Tests for S4 symbol detection module

testthat::test_that("extract_s4_symbols returns empty for non-S4 code", {
  code <- "x <- 1\nfoo <- function(a) a + 1"
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 0)
  unlink(tmp)
})

testthat::test_that("extract_s4_symbols detects setClass", {
  code <- 'setClass("Person", slots = c(name = "character", age = "numeric"))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result[[1]]$type, "s4_class")
  testthat::expect_equal(result[[1]]$class_name, "Person")
  testthat::expect_true("name" %in% result[[1]]$slots)
  testthat::expect_true("age" %in% result[[1]]$slots)
  unlink(tmp)
})

testthat::test_that("extract_s4_symbols detects setClass with contains", {
  code <- 'setClass("Employee", contains = "Person", slots = c(salary = "numeric"))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result[[1]]$type, "s4_class")
  testthat::expect_equal(result[[1]]$class_name, "Employee")
  testthat::expect_true("Person" %in% result[[1]]$contains)
  unlink(tmp)
})

testthat::test_that("extract_s4_symbols detects setGeneric", {
  code <- 'setGeneric("show", function(object) standardGeneric("show"))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result[[1]]$type, "s4_generic")
  testthat::expect_equal(result[[1]]$generic_name, "show")
  unlink(tmp)
})

testthat::test_that("extract_s4_symbols detects setMethod", {
  code <- 'setMethod("show", "Person", function(object) cat(object@name))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 1)
  testthat::expect_equal(result[[1]]$type, "s4_method")
  testthat::expect_equal(result[[1]]$generic_name, "show")
  testthat::expect_equal(result[[1]]$signature, "Person")
  unlink(tmp)
})

testthat::test_that("extract_s4_symbols handles multiple S4 definitions", {
  code <- '
setClass("A", slots = c(x = "numeric"))
setClass("B", slots = c(y = "character"))
setGeneric("process", function(obj) standardGeneric("process"))
setMethod("process", "A", function(obj) obj@x * 2)
setMethod("process", "B", function(obj) paste0(obj@y, "!"))
'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  result <- extract_s4_symbols(ast)
  testthat::expect_equal(length(result), 5)
  
  types <- sapply(result, function(x) x$type)
  testthat::expect_equal(sum(types == "s4_class"), 2)
  testthat::expect_equal(sum(types == "s4_generic"), 1)
  testthat::expect_equal(sum(types == "s4_method"), 2)
  unlink(tmp)
})

testthat::test_that("build_s4_symbol_entries creates proper entries", {
  code <- 'setClass("TestClass", slots = c(value = "numeric"))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  entries <- build_s4_symbol_entries(ast, basename(tmp), NULL)
  
  testthat::expect_true(length(entries) >= 1)
  class_entry <- entries[[1]]
  testthat::expect_equal(class_entry$kind, "s4_class")
  testthat::expect_equal(class_entry$display_name, "TestClass")
  
  slot_entries <- Filter(function(e) e$kind == "s4_slot", entries)
  testthat::expect_equal(length(slot_entries), 1)
  testthat::expect_equal(slot_entries[[1]]$display_name, "value")
  unlink(tmp)
})

testthat::test_that("S4 symbols integrate into resolve_symbols", {
  code <- 'setClass("MyClass", slots = c(data = "list"))'
  tmp <- tempfile(fileext = ".R")
  writeLines(code, tmp)
  ast <- parse_file(list(path = tmp, relative_path = basename(tmp)))
  workspace <- list(metadata = list(namespace = NULL), dependencies = list())
  
  result <- resolve_symbols(ast, workspace, track_externals = FALSE)
  
  s4_symbols <- Filter(function(s) grepl("^s4_", s$kind), result$symbols)
  testthat::expect_true(length(s4_symbols) >= 1)
  unlink(tmp)
})
