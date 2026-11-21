# Same-named top-level functions in different files need one symbol per file.

duplicate_fixture <- function(files) {
  parent <- tempfile("scip duplicates ")
  root <- file.path(parent, "dupes")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c("Package: dupes", "Version: 1.0.0"), file.path(root, "DESCRIPTION"))
  for (name in names(files)) writeLines(files[[name]], file.path(root, "R", name))
  list(root = root, parent = parent)
}

resolve_duplicate_fixture <- function(fixture) {
  workspace <- analyze_workspace(fixture$root)
  files <- discover_files(workspace, include_patterns = "R/**/*.R")$files
  parsed <- lapply(files, parse_file)
  table <- build_symbol_table(parsed, workspace)
  results <- lapply(parsed, function(ast) {
    resolve_symbols(ast, workspace, symbol_table = table,
                    tree_index = table$tree_indices[[ast$file_info$relative_path]])
  })
  names(results) <- vapply(parsed, function(ast) ast$file_info$relative_path, character(1))
  list(table = table, results = results)
}

# Symbol of the single occurrence of `name` on 1-based `line`.
duplicate_symbol <- function(result, lines, name, line) {
  rows <- Filter(function(occ) {
    occ$range[1] == line - 1L &&
      identical(substr(lines[line], occ$range[2] + 1L, occ$range[4]), name)
  }, result$occurrences)
  expect_equal(length(rows), 1L, info = paste(name, "at line", line))
  rows[[1]]$symbol
}

a_lines <- c(".section <- function(x) x", "use_a <- function() .section(1)")
b_lines <- c(".section <- function(x) x + 1", "use_b <- function() .section(2)")
c_lines <- c("use_c <- function() .section(3)")

test_that("same top-level name in two files gets one symbol per file", {
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- duplicate_fixture(list("a.R" = a_lines, "b.R" = b_lines))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      resolved <- resolve_duplicate_fixture(fixture)
      a_def <- duplicate_symbol(resolved$results[["R/a.R"]], a_lines, ".section", 1L)
      a_use <- duplicate_symbol(resolved$results[["R/a.R"]], a_lines, ".section", 2L)
      b_def <- duplicate_symbol(resolved$results[["R/b.R"]], b_lines, ".section", 1L)
      b_use <- duplicate_symbol(resolved$results[["R/b.R"]], b_lines, ".section", 2L)
      expect_false(identical(a_def, b_def))
      expect_identical(a_use, a_def)
      expect_identical(b_use, b_def)
      expect_identical(a_def, "scip-r r dupes 1.0.0 `.section`(R:a.R).")
    })
  }
})

test_that("a name defined in one file keeps its plain ID across files", {
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- duplicate_fixture(list(
        "a.R" = c("helper <- function(x) x"),
        "b.R" = c("use_b <- function() helper(1)")
      ))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      resolved <- resolve_duplicate_fixture(fixture)
      def <- duplicate_symbol(resolved$results[["R/a.R"]], "helper <- function(x) x", "helper", 1L)
      use <- duplicate_symbol(resolved$results[["R/b.R"]], "use_b <- function() helper(1)", "helper", 1L)
      expect_identical(def, "scip-r r dupes 1.0.0 helper().")
      expect_identical(use, def)
    })
  }
})

test_that("a caller without its own definition of a duplicated name stays unresolved", {
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- duplicate_fixture(list("a.R" = a_lines, "b.R" = b_lines, "c.R" = c_lines))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      resolved <- resolve_duplicate_fixture(fixture)
      ids <- vapply(resolved$table$definitions[c("R/a.R\t.section", "R/b.R\t.section")],
                    function(def) def$symbol_id, character(1))
      expect_null(resolved$table$definitions_env[[".section"]])
      use <- duplicate_symbol(resolved$results[["R/c.R"]], c_lines, ".section", 1L)
      expect_false(use %in% ids)
      expect_match(use, "^local ")
    })
  }
})
