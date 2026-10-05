# Architext-2t0js: lexical definitions and references must share an identity,
# without capturing an unrelated package definition or a sibling closure.

binding_fixture <- function(files) {
  parent <- tempfile("scip bindings ")
  root <- file.path(parent, "nestedbindings")
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(c("Package: nestedbindings", "Version: 1.0.0"), file.path(root, "DESCRIPTION"))
  for (name in names(files)) writeLines(files[[name]], file.path(root, "R", name))
  list(root = root, parent = parent, files = files)
}

resolve_binding_fixture <- function(fixture) {
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

binding_token <- function(result, lines, name, line) {
  rows <- Filter(function(occ) {
    occ$range[1] == line - 1L &&
      identical(substr(lines[line], occ$range[2] + 1L, occ$range[4]), name)
  }, result$occurrences)
  expect_equal(length(rows), 1L, info = paste(name, "at line", line))
  if (length(rows) != 1L) return(list(symbol = NA_character_, role = NA_character_))
  rows[[1]]
}

test_that("local function bindings shadow package and base names in both backends", {
  lines <- c(
    "print.value <- function(x) x",
    "outer <- function(x) {",
    "  print.value <- function(y) {",
    "    print.value(y)",
    "  }",
    "  print <- function(y) y",
    "  print(x)",
    "  function() print.value(x)",
    "  nestedbindings::print.value(x)",
    "}"
  )
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- binding_fixture(list("definitions.R" = lines))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      resolved <- resolve_binding_fixture(fixture)
      result <- resolved$results[["R/definitions.R"]]
      global <- resolved$table$definitions[["print.value"]]$symbol_id
      local <- binding_token(result, lines, "print.value", 3L)
      expect_identical(local$role, "definition")
      expect_false(identical(local$symbol, global))
      expect_match(local$symbol, "^local ")
      for (line in c(4L, 8L)) {
        occurrence <- binding_token(result, lines, "print.value", line)
        expect_identical(occurrence$symbol, local$symbol)
        expect_identical(occurrence$role, "reference")
      }
      expect_identical(binding_token(result, lines, "print.value", 9L)$symbol, global)
      print_id <- binding_token(result, lines, "print", 6L)$symbol
      expect_identical(binding_token(result, lines, "print", 7L)$symbol, print_id)
      expect_match(print_id, "^local ")
      rows <- Filter(function(s) identical(s$display_name, "print.value"), result$symbols)
      expect_length(rows, 2L)
      expect_setequal(vapply(rows, function(s) s$symbol, character(1)), c(global, local$symbol))
    })
  }
})

test_that("same named nested scopes keep distinct function bindings and captured calls", {
  lines <- c(
    "print.value <- function(x) x",
    "first <- function(x) {",
    "  inner <- function() {",
    "    print.value <- function(y) y",
    "    function() print.value(x)",
    "  }",
    "}",
    "second <- function(x) {",
    "  inner <- function() {",
    "    print.value <- function(y) y",
    "    function() print.value(x)",
    "  }",
    "}"
  )
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- binding_fixture(list("definitions.R" = lines))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      result <- resolve_binding_fixture(fixture)$results[["R/definitions.R"]]
      first <- binding_token(result, lines, "print.value", 4L)
      second <- binding_token(result, lines, "print.value", 10L)
      expect_identical(first$role, "definition")
      expect_identical(second$role, "definition")
      expect_false(identical(first$symbol, second$symbol))
      expect_identical(binding_token(result, lines, "print.value", 5L)$symbol, first$symbol)
      expect_identical(binding_token(result, lines, "print.value", 11L)$symbol, second$symbol)
      rows <- Filter(function(s) identical(s$display_name, "print.value"), result$symbols)
      expect_length(rows, 3L)
      expect_length(unique(vapply(rows, function(s) s$symbol, character(1))), 3L)
    })
  }
})

test_that("lexical calls do not capture cross file package callers or unshadowed closures", {
  definitions <- c("print.value <- function(x) x")
  local <- c(
    "local_user <- function(x) {",
    "  print.value <- function(y) y",
    "  print.value(x)",
    "  print.value",
    "}",
    "global_user <- function(x) {",
    "  print.value(x)",
    "  function() print.value(x)",
    "}"
  )
  calls <- c("cross_file <- function(x) print.value(x)")
  for (backend in c("R", "cpp")) {
    with_resolver_backend(backend, function() {
      fixture <- binding_fixture(list("definitions.R" = definitions, "local.R" = local, "use.R" = calls))
      on.exit(unlink(fixture$parent, recursive = TRUE), add = TRUE)
      resolved <- resolve_binding_fixture(fixture)
      global <- resolved$table$definitions[["print.value"]]$symbol_id
      local_result <- resolved$results[["R/local.R"]]
      local_id <- binding_token(local_result, local, "print.value", 2L)$symbol
      expect_false(identical(local_id, global))
      for (line in c(3L, 4L)) expect_identical(binding_token(local_result, local, "print.value", line)$symbol, local_id)
      for (line in c(7L, 8L)) expect_identical(binding_token(local_result, local, "print.value", line)$symbol, global)
      cross_file <- binding_token(resolved$results[["R/use.R"]], calls, "print.value", 1L)
      expect_identical(cross_file$symbol, global)
      expect_identical(cross_file$role, "reference")
      expect_identical(binding_token(resolved$results[["R/definitions.R"]], definitions, "print.value", 1L)$role,
                       "definition")
    })
  }
})
