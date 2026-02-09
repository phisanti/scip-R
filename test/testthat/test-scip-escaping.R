# Tests for SCIP identifier escaping (backtick-quoting per SCIP spec).
# R uses dots in identifiers (e.g. pgx.createPGX, as.data.frame) but SCIP
# treats dots as descriptor delimiters. Names with dots must be backtick-quoted.

# =============================================================================
# escape_scip_identifier
# =============================================================================

test_that("escape_scip_identifier passes through plain names", {
  expect_equal(escape_scip_identifier("foo"), "foo")
  expect_equal(escape_scip_identifier("createPGX"), "createPGX")
  expect_equal(escape_scip_identifier("my_var"), "my_var")
  expect_equal(escape_scip_identifier("x1"), "x1")
})

test_that("escape_scip_identifier passes through valid SCIP special chars", {
  # _ + - $ are valid identifier characters per SCIP spec
  expect_equal(escape_scip_identifier("a_b"), "a_b")
  expect_equal(escape_scip_identifier("a+b"), "a+b")
  expect_equal(escape_scip_identifier("a-b"), "a-b")
  expect_equal(escape_scip_identifier("a$b"), "a$b")
})

test_that("escape_scip_identifier backtick-quotes dotted names", {
  expect_equal(escape_scip_identifier("pgx.createPGX"), "`pgx.createPGX`")
  expect_equal(escape_scip_identifier("as.data.frame"), "`as.data.frame`")
  expect_equal(escape_scip_identifier("is.na"), "`is.na`")
  expect_equal(escape_scip_identifier("PGX.DIR"), "`PGX.DIR`")
})

test_that("escape_scip_identifier backtick-quotes names with spaces", {
  expect_equal(escape_scip_identifier("my func"), "`my func`")
})

test_that("escape_scip_identifier doubles internal backticks", {
  expect_equal(escape_scip_identifier("foo`bar"), "`foo``bar`")
})

# =============================================================================
# build_global_symbol_id — dotted names
# =============================================================================

test_that("build_global_symbol_id escapes dotted function names", {
  sym <- build_global_symbol_id("pgx.createPGX", "playbase", "1.0.0", "function")
  expect_equal(sym, "scip-r r playbase 1.0.0 `pgx.createPGX`().")
})

test_that("build_global_symbol_id escapes dotted variable names", {
  sym <- build_global_symbol_id("PGX.DIR", "pkg", "1.0.0", "variable")
  expect_equal(sym, "scip-r r pkg 1.0.0 `PGX.DIR`.")
})

test_that("build_global_symbol_id escapes dotted S3 methods", {
  sym <- build_global_symbol_id("mean.data.frame", "base", "4.3.2", "s3_method")
  expect_equal(sym, "scip-r r base 4.3.2 `mean.data.frame`().")
})

test_that("build_global_symbol_id leaves plain names unescaped", {
  sym <- build_global_symbol_id("createPGX", "playbase", "1.0.0", "function")
  expect_equal(sym, "scip-r r playbase 1.0.0 createPGX().")
})

# =============================================================================
# build_local_symbol_id — dotted names
# =============================================================================

test_that("build_local_symbol_id preserves dots via backtick-quoting", {
  sym <- build_local_symbol_id("has.pgx", "R/server.R")
  expect_match(sym, "`has.pgx`", fixed = TRUE)
  expect_true(!grepl("has_pgx", sym))
})

test_that("build_local_symbol_id with scope escapes both name and scope", {
  sym <- build_local_symbol_id("renv.out", "dev/functions.R", "install.pkgs")
  expect_match(sym, "`renv.out`", fixed = TRUE)
  expect_match(sym, "`install.pkgs`", fixed = TRUE)
})

test_that("build_local_symbol_id leaves plain names unescaped", {
  sym <- build_local_symbol_id("counter", "R/utils.R", "main")
  expect_true(!grepl("`", sym))
  expect_match(sym, "counter", fixed = TRUE)
})
