# S3 metadata must annotate the identity whose definition and
# references the main resolver already emitted, never a second local identity.

with_s3_identity_backend <- with_resolver_backend

s3_identity_project <- function(lines = c(
  ".compute_ci <- function(x) x",
  "#' Format the result.",
  "#' @export",
  "format.summary_result <- function(x) x",
  "print.summary_result <- function(x) format.summary_result(x)",
  "summarize <- function(x) UseMethod('summarize')"
)) {
  root <- tempfile("scip_s3_identity_")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c("Package: s3identity", "Version: 1.0.0", "Title: S3 Identity Fixture"), file.path(root, "DESCRIPTION"))
  writeLines("export(format.summary_result)", file.path(root, "NAMESPACE"))
  writeLines(lines, file.path(root, "R", "definitions.R"))
  writeLines("consume <- function(x) print.summary_result(x)", file.path(root, "R", "use.R"))
  root
}

s3_identity_resolve <- function(root) {
  workspace <- analyze_workspace(root)
  plan <- discover_files(workspace, include_patterns = "R/**/*.R")
  parsed <- lapply(plan$files, parse_file)
  table <- build_symbol_table(parsed, workspace)
  results <- lapply(parsed, function(ast) {
    resolve_symbols(
      ast, workspace, symbol_table = table,
      tree_index = table$tree_indices[[ast$file_info$relative_path]]
    )
  })
  names(results) <- vapply(parsed, function(ast) ast$file_info$relative_path, character(1))
  list(table = table, results = results)
}

testthat::test_that("S3 metadata reuses located package identities", {
  for (backend in c("R", "cpp")) {
    with_s3_identity_backend(backend, function() {
      root <- s3_identity_project()
      on.exit(unlink(root, recursive = TRUE), add = TRUE)
      resolved <- s3_identity_resolve(root)
      main <- resolved$results[["R/definitions.R"]]
      kinds <- c(
        ".compute_ci" = "function", "format.summary_result" = "s3_method",
        "print.summary_result" = "s3_method", "summarize" = "s3_generic"
      )
      for (name in names(kinds)) {
        expected_id <- resolved$table$definitions[[name]]$symbol_id
        rows <- Filter(function(symbol) identical(symbol$display_name, name), main$symbols)
        testthat::expect_length(rows, 1)
        canonical <- Filter(function(symbol) identical(symbol$symbol, expected_id), rows)
        testthat::expect_length(canonical, 1)
        testthat::expect_identical(canonical[[1]]$kind, unname(kinds[[name]]))
        definitions <- Filter(function(occ) {
          identical(occ$symbol, expected_id) && identical(occ$role, "definition")
        }, main$occurrences)
        testthat::expect_length(definitions, 1)
      }
      format_id <- resolved$table$definitions[["format.summary_result"]]$symbol_id
      row <- Find(function(symbol) identical(symbol$symbol, format_id), main$symbols)
      testthat::expect_true(any(grepl("@export", row$documentation, fixed = TRUE)))
      print_id <- resolved$table$definitions[["print.summary_result"]]$symbol_id
      uses <- Filter(function(occ) identical(occ$symbol, print_id), resolved$results[["R/use.R"]]$occurrences)
      testthat::expect_length(uses, 1)
      testthat::expect_identical(uses[[1]]$role, "reference")
    })
  }
})

testthat::test_that("S3 annotations keep same-named nested definitions separate", {
  for (backend in c("R", "cpp")) {
    with_s3_identity_backend(backend, function() {
      root <- s3_identity_project(c(
        "first <- function(x) {",
        "  print.value <- function(y) y",
        "  print.value(x)",
        "}",
        "second <- function(x) {",
        "  print.value <- function(y) y",
        "  print.value(x)",
        "}",
        ".dispatch <- function(x) UseMethod('.dispatch')",
        ".dispatch.value <- function(x) x"
      ))
      on.exit(unlink(root, recursive = TRUE), add = TRUE)
      main <- s3_identity_resolve(root)$results[["R/definitions.R"]]
      rows <- Filter(function(symbol) identical(symbol$display_name, "print.value"), main$symbols)
      testthat::expect_length(rows, 2)
      testthat::expect_equal(length(unique(vapply(rows, function(symbol) symbol$symbol, character(1)))), 2)
      for (row in rows) {
        testthat::expect_identical(row$kind, "s3_method")
        definitions <- Filter(function(occ) {
          identical(occ$symbol, row$symbol) && identical(occ$role, "definition")
        }, main$occurrences)
        testthat::expect_length(definitions, 1)
      }
      for (name in c(".dispatch", ".dispatch.value")) {
        rows <- Filter(function(symbol) identical(symbol$display_name, name), main$symbols)
        testthat::expect_length(rows, 1)
      }
      parsed <- parse_s3_method_name(".dispatch.value")
      testthat::expect_identical(parsed$generic_name, ".dispatch")
      testthat::expect_identical(parsed$class_name, "value")
      testthat::expect_false(parse_s3_method_name(".compute_ci")$is_method)
    })
  }
})

testthat::test_that("serialized S3 symbols have one located identity and preserved documentation", {
  for (backend in c("R", "cpp")) {
    with_s3_identity_backend(backend, function() {
      root <- s3_identity_project()
      on.exit(unlink(root, recursive = TRUE), add = TRUE)
      output <- file.path(root, "index.scip")
      index_project(list(root = root, output = output, include = "R/**/*.R", cache = FALSE, parallel = 0L))
      doc <- scipr_document(scipr_read_index(output), "R/definitions.R")
      kinds <- c(".compute_ci" = "Function", "format.summary_result" = "Method", "print.summary_result" = "Method")
      for (name in names(kinds)) {
        rows <- Filter(function(symbol) identical(symbol$display_name, name), as.list(doc$symbols))
        testthat::expect_length(rows, 1)
        canonical_id <- build_global_symbol_id(name, basename(root), "1.0.0", "function")
        row <- Find(function(symbol) identical(symbol$symbol, canonical_id), rows)
        testthat::expect_false(is.null(row))
        testthat::expect_equal(row$kind, scipr_symbol_kind(kinds[[name]]))
        definitions <- scipr_occurrences_for(doc, canonical_id, "definition")
        testthat::expect_length(definitions, 1)
        testthat::expect_true(all(definitions[[1]]$range >= 0L))
      }
      format_id <- build_global_symbol_id("format.summary_result", basename(root), "1.0.0", "function")
      row <- Find(function(symbol) identical(symbol$symbol, format_id), as.list(doc$symbols))
      testthat::expect_true(any(grepl("@export", row$documentation, fixed = TRUE)))
      helper_id <- build_global_symbol_id(".compute_ci", basename(root), "1.0.0", "function")
      testthat::expect_equal(scipr_occurrences_for(doc, helper_id, "definition")[[1]]$range[1], 0L)
    })
  }
})
