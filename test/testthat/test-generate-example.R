# Real example entry points must keep source scope and caller isolation.
example_script <- file.path(project_root, "example", "generate_scip_index.R")

load_index_example <- function() {
  env <- new.env(parent = globalenv())
  source(example_script, local = env)
  env
}

example_fixture <- function(root) {
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "scripts"))
  writeLines(c(
    "Package: exampleprobe", "Version: 0.0.1", "Title: Synthetic Example",
    "Description: A bounded example fixture.", "License: MIT"
  ), file.path(root, "DESCRIPTION"))
  writeLines("answer <- function(x) x + 1", file.path(root, "R", "probe.R"))
  writeLines("ignored <- function() 99", file.path(root, "scripts", "outside.R"))
}

check_example_index <- function(output) {
  expect_true(file.exists(output))
  index <- scipr_read_index(output)
  expect_length(index$documents, 1L)
  doc <- scipr_document(index, "R/probe.R")
  names <- vapply(as.list(doc$symbols), function(s) s$display_name, character(1))
  expect_true("answer" %in% names)
  expect_false("ignored" %in% names)
}

test_that("sourced self-index example calls the current bounded configuration API", {
  env <- load_index_example()
  env$discover_script_path <- function() example_script
  cfg <- NULL
  env$source_project_modules <- function(root) {
    list(index_project = function(value) cfg <<- value)
  }
  output <- tempfile("scip example ", fileext = ".scip")
  expected_output <- file.path(normalizePath(dirname(output)), basename(output))
  expect_identical(env$generate_self_index(output), expected_output)
  expect_identical(cfg$root, project_root)
  expect_identical(cfg$include, "R/**/*.R")
  expect_identical(cfg$output, expected_output)
  expect_identical(cfg$parallel, 0L)
  expect_false(cfg$cache)
})

test_that("sourced example indexes a synthetic project without changing caller bindings", {
  tmp <- tempfile("scip example ")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  fixture <- file.path(tmp, "project")
  example_fixture(fixture)
  output <- file.path(tmp, "source index.scip")
  old_wd <- setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  env <- load_index_example()
  before <- parse_file
  expect_identical(
    env$generate_self_index("source index.scip", project_root = fixture),
    file.path(normalizePath(tmp), "source index.scip")
  )
  expect_identical(parse_file, before)
  check_example_index(output)
})

test_that("direct example execution honors output and project paths with spaces", {
  tmp <- tempfile("scip example ")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  fixture <- file.path(tmp, "project with spaces")
  example_fixture(fixture)
  output <- file.path(tmp, "direct index.scip")
  log <- file.path(tmp, "child.log")
  old_wd <- setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  status <- suppressWarnings(system2(
    file.path(R.home("bin"), "Rscript"),
    c("--vanilla", shQuote(example_script), shQuote("direct index.scip"), shQuote(fixture)),
    stdout = log, stderr = log
  ))
  expect_equal(status, 0L, info = paste(readLines(log), collapse = "\n"))
  if (status == 0L) check_example_index(output)
})
