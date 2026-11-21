# Every indexed file gets one module symbol so top-level script calls can be
# attributed to their file downstream.

module_fixture <- function() {
  parent <- tempfile("scip module ")
  root <- file.path(parent, "modpkg")
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "inst", "scripts"), recursive = TRUE)
  writeLines(c("Package: modpkg", "Version: 2.0.0"), file.path(root, "DESCRIPTION"))
  writeLines(c("helper <- function(x) x + 1", "other <- function() 2"), file.path(root, "R", "lib.R"))
  writeLines(c("# script", "result <- helper(1)", "print(result)"), file.path(root, "inst", "scripts", "run-it.R"))
  list(root = root, parent = parent)
}

module_index <- function(root) {
  output <- tempfile(fileext = ".scip")
  index_project(list(
    root = root, output = output, include = c("R/**/*.R", "inst/**/*.R"), parallel = 0L, cache = FALSE
  ))
  scipr_read_index(output)
}

test_that("every indexed file has one module symbol with a definition occurrence", {
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- module_fixture()
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      index <- module_index(fixture$root)
      expected <- c(
        "R/lib.R" = "scip-r r modpkg 2.0.0 R/`lib.R`/",
        "inst/scripts/run-it.R" = "scip-r r modpkg 2.0.0 inst/scripts/`run-it.R`/"
      )
      kind_module <- as.integer(RProtoBuf::P("scip.SymbolInformation.Kind")$Module)
      definition <- as.integer(RProtoBuf::P("scip.SymbolRole")$Definition)
      for (path in names(expected)) {
        document <- scipr_document(index, path)
        info <- Filter(function(s) identical(s$symbol, expected[[path]]), as.list(document$symbols))
        expect_length(info, 1L)
        expect_identical(as.integer(info[[1]]$kind), kind_module)
        expect_identical(info[[1]]$display_name, path)
        occurrences <- scipr_occurrences_for(document, expected[[path]], "definition")
        expect_length(occurrences, 1L)
        expect_identical(as.integer(occurrences[[1]]$range), c(0L, 0L, 0L, 0L))
        expect_identical(
          as.integer(occurrences[[1]]$enclosing_range)[1:2], c(0L, 0L)
        )
        expect_true(bitwAnd(as.integer(occurrences[[1]]$symbol_roles), definition) != 0L)
      }
      script <- scipr_occurrences_for(scipr_document(index, "inst/scripts/run-it.R"),
                                      expected[["inst/scripts/run-it.R"]])
      expect_identical(as.integer(script[[1]]$enclosing_range), c(0L, 0L, 2L, 13L))
    })
  }
})

test_that("module symbols leave existing symbol IDs and top-level references alone", {
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- module_fixture()
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      index <- module_index(fixture$root)
      lib <- scipr_document(index, "R/lib.R")
      helper <- "scip-r r modpkg 2.0.0 helper()."
      expect_length(scipr_occurrences_for(lib, helper, "definition"), 1L)
      script <- scipr_document(index, "inst/scripts/run-it.R")
      refs <- scipr_occurrences_for(script, helper, "reference")
      expect_length(refs, 1L)
      expect_identical(as.integer(refs[[1]]$range), c(1L, 10L, 1L, 16L))
      # Only the two module symbols are new: no other descriptor ends in "/".
      ids <- unlist(lapply(index$documents, function(d) vapply(d$symbols, function(s) s$symbol, "")))
      expect_length(grep("/$", ids), 2L)
    })
  }
})

test_that("an empty file still gets a module symbol", {
  fixture <- module_fixture()
  on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
  file.create(file.path(fixture$root, "R", "empty.R"))
  document <- scipr_document(module_index(fixture$root), "R/empty.R")
  expect_identical(
    as.integer(document$occurrences[[1]]$enclosing_range), c(0L, 0L, 0L, 0L)
  )
})
