# This test exercises file discovery for minimal R projects.
# It creates a temporary file and ensures discover_files queues it.
# Future refinements should cover ignore patterns and caching.

testthat::test_that("discover_files finds R source", {
  tmp_dir <- tempfile("scip_r_files_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  file_path <- file.path(tmp_dir, "script.R")
  writeLines("x <- 1", file_path)

  workspace <- list(
    metadata = list(project_root = tmp_dir),
    ignore = character()
  )

  plan <- discover_files(workspace, cache_enabled = FALSE)
  testthat::expect_equal(length(plan$files), 1)
  testthat::expect_match(plan$files[[1]]$relative_path, "script.R")
})

# =============================================================================
# Directory-pruning walk
# =============================================================================
#
# discover_files used list.files(recursive = TRUE) which descended the entire
# tree (incl. renv/) before filtering. The pruning walk skips out-of-scope
# subtrees up front. These tests pin that the PRUNED candidate set equals the
# UNPRUNED set after filtering — a discovery-speed change must not alter which
# files are indexed. They would fail if the walk pruned a directory that can
# contain a kept file, or if it stopped recognizing .r / nested files.

# Build a fixture tree:
#   R/foo.R                  (in scope)
#   R/sub/deep/bar.R         (in scope, nested under **)
#   R/legacy.r               (in scope, lowercase extension)
#   renv/library/pkg/R/x.R   (OUT of scope — must be pruned, never matched)
#   data/raw.R               (OUT of scope under include "R/**/*.[Rr]")
make_scoped_workspace <- function(ignore = list()) {
  tmp_dir <- tempfile("scip_r_prune_")
  dirs <- c("R/sub/deep", "renv/library/pkg/R", "data")
  for (d in dirs) dir.create(file.path(tmp_dir, d), recursive = TRUE)
  writeLines("x <- 1", file.path(tmp_dir, "R", "foo.R"))
  writeLines("y <- 2", file.path(tmp_dir, "R", "sub", "deep", "bar.R"))
  writeLines("z <- 3", file.path(tmp_dir, "R", "legacy.r"))
  writeLines("dep <- 1", file.path(tmp_dir, "renv", "library", "pkg", "R", "x.R"))
  writeLines("raw <- 1", file.path(tmp_dir, "data", "raw.R"))
  list(
    metadata = list(project_root = tmp_dir),
    ignore = ignore
  )
}

testthat::test_that("pruning walk skips out-of-scope subtrees but keeps all in-scope files", {
  ws <- make_scoped_workspace()
  on.exit(unlink(ws$metadata$project_root, recursive = TRUE), add = TRUE)

  plan <- discover_files(ws, cache_enabled = FALSE,
                         include_patterns = c("R/**/*.R", "R/**/*.r"))
  got <- sort(vapply(plan$files, function(f) f$relative_path, character(1)))

  testthat::expect_equal(
    got,
    c("R/foo.R", "R/legacy.r", "R/sub/deep/bar.R")
  )
  # renv/ and data/ must never appear — they were pruned during the walk.
  testthat::expect_false(any(grepl("^renv/", got)))
  testthat::expect_false(any(grepl("^data/", got)))
})

testthat::test_that("pruned result equals the unpruned-then-filtered result", {
  # Equivalence guarantee: pruning only drops candidates the post-walk filter
  # would discard anyway. We reproduce the legacy behaviour (full list.files +
  # include filter) and compare.
  ws <- make_scoped_workspace()
  root <- ws$metadata$project_root
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  includes <- c("R/**/*.R", "R/**/*.r")

  pruned <- discover_files(ws, cache_enabled = FALSE, include_patterns = includes)
  pruned_rel <- sort(vapply(pruned$files, function(f) f$relative_path, character(1)))

  all_files <- sort(list.files(root, pattern = "\\.[Rr]$", recursive = TRUE))
  regexes <- compile_include_patterns(includes)
  legacy_rel <- sort(all_files[vapply(all_files, matches_any_pattern, logical(1), regexes)])

  testthat::expect_equal(pruned_rel, legacy_rel)
})

testthat::test_that("dir_only gitignore rule prunes a subtree (no negation)", {
  rules <- parse_gitignore_lines(c("renv/"))
  ws <- make_scoped_workspace(ignore = rules)
  on.exit(unlink(ws$metadata$project_root, recursive = TRUE), add = TRUE)

  # No include patterns: rely on the gitignore dir-only prune for renv/.
  plan <- discover_files(ws, cache_enabled = FALSE)
  got <- sort(vapply(plan$files, function(f) f$relative_path, character(1)))
  testthat::expect_false(any(grepl("^renv/", got)))
  testthat::expect_true("R/foo.R" %in% got)
})

# Direct unit tests for the prefix matcher — never prune an ancestor of a match.
testthat::test_that("dir_could_contain_include keeps every ancestor of a match", {
  parsed <- parse_include_segments(c("components/**/*.R"))
  testthat::expect_true(dir_could_contain_include("components", parsed))
  testthat::expect_true(dir_could_contain_include("components/board", parsed))
  testthat::expect_true(dir_could_contain_include("components/board/deep", parsed))
  # Unrelated top-level dir cannot contain a match -> safe to prune.
  testthat::expect_false(dir_could_contain_include("renv", parsed))
  testthat::expect_false(dir_could_contain_include("renv/library", parsed))
})

testthat::test_that("dir_could_contain_include handles a literal-prefix pattern", {
  parsed <- parse_include_segments(c("R/foo.R"))
  testthat::expect_true(dir_could_contain_include("R", parsed))
  testthat::expect_false(dir_could_contain_include("inst", parsed))
})
