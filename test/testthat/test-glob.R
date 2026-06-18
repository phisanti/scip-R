# Tests for glob_to_regex (R/workspace.R) — the hand-rolled glob->regex used by
# both include-pattern matching (compile_include_patterns) and gitignore parsing
# (parse_gitignore_line). It replaced utils::glob2rx and changed "**" semantics,
# so these tests pin the behavior glob2rx does NOT provide.
#
# Bead: scip-R-hee. Regression net for the discover_files pruning walk
# (scip-R-gpa), which depends on the same matcher via parse_include_segments.

# Helper: does the compiled glob match this (relative) path?
glob_hits <- function(pattern, path) {
  grepl(glob_to_regex(pattern), path, perl = TRUE)
}

testthat::test_that("single * matches within a path segment but not across /", {
  testthat::expect_true(glob_hits("*.R", "foo.R"))
  testthat::expect_true(glob_hits("test_*", "test_utils"))
  testthat::expect_true(glob_hits("*.R", ".R")) # zero-width * is allowed

  # A single * must NOT cross a directory separator.
  testthat::expect_false(glob_hits("*.R", "sub/foo.R"))
  testthat::expect_false(glob_hits("a*b", "a/b"))
})

testthat::test_that("? matches exactly one non-slash character", {
  testthat::expect_true(glob_hits("foo?.R", "foo1.R"))
  testthat::expect_false(glob_hits("foo?.R", "foo.R"))   # needs a char
  testthat::expect_false(glob_hits("foo?.R", "foo12.R")) # only one
  testthat::expect_false(glob_hits("a?b", "a/b"))        # not the separator
})

testthat::test_that("trailing ** (no slash) matches across segments", {
  testthat::expect_true(glob_hits("R/**", "R/foo.R"))
  testthat::expect_true(glob_hits("R/**", "R/sub/deep/foo.R"))
})

testthat::test_that("**/ matches zero or more directory segments", {
  # The fix in commit 7dfb13b: "R/**/*.R" must match a top-level file directly
  # under R/ (zero intermediate dirs), not only nested ones. glob2rx anchored
  # "**" too strictly and dropped R/foo.R.
  testthat::expect_true(glob_hits("R/**/*.R", "R/foo.R"))            # zero dirs
  testthat::expect_true(glob_hits("R/**/*.R", "R/sub/foo.R"))        # one dir
  testthat::expect_true(glob_hits("R/**/*.R", "R/a/b/c/foo.R"))      # many dirs

  # Still anchored to the R/ prefix and the .R suffix.
  testthat::expect_false(glob_hits("R/**/*.R", "other/foo.R"))
  testthat::expect_false(glob_hits("R/**/*.R", "R/foo.py"))
})

testthat::test_that("components/**/*.R behaves like the project include scope", {
  testthat::expect_true(glob_hits("components/**/*.R", "components/foo.R"))
  testthat::expect_true(glob_hits("components/**/*.R", "components/board/plot.R"))
  testthat::expect_false(glob_hits("components/**/*.R", "renv/library/pkg/R/x.R"))
})

testthat::test_that("regex metacharacters in a glob are matched literally", {
  # A literal dot must not act as regex "any char".
  testthat::expect_true(glob_hits("a.b", "a.b"))
  testthat::expect_false(glob_hits("a.b", "axb"))

  # Parens, plus, dollar, braces, caret, pipe are literal.
  testthat::expect_true(glob_hits("a+b", "a+b"))
  testthat::expect_false(glob_hits("a+b", "aaab"))
  testthat::expect_true(glob_hits("f(x)", "f(x)"))
  testthat::expect_true(glob_hits("a{b}", "a{b}"))
  testthat::expect_true(glob_hits("a|b", "a|b"))
  testthat::expect_false(glob_hits("a|b", "a"))
})

testthat::test_that("glob is fully anchored (^...$)", {
  testthat::expect_true(glob_hits("foo", "foo"))
  testthat::expect_false(glob_hits("foo", "foobar"))
  testthat::expect_false(glob_hits("foo", "barfoo"))
})

testthat::test_that("empty pattern compiles to a match-anything regex", {
  testthat::expect_equal(glob_to_regex(""), "^.*$")
})
