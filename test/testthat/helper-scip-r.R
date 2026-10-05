# This helper sources implementation scripts for the scip-r tests.
# It guarantees CLI and indexing modules are loaded into the test environment.
# Future refactors should replace manual sourcing with a package namespace.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat must be installed to run helper setup.")
}

project_root <- normalizePath(file.path(testthat::test_path("."), "..", ".."), winslash = "/", mustWork = TRUE)
r_dir <- file.path(project_root, "R")
r_proto <- normalizePath(
  file.path(project_root, "..", "..", "..", "references", "scip", "scip.proto"),
  winslash = "/",
  mustWork = FALSE
)
if (file.exists(r_proto)) {
  Sys.setenv(SCIP_PROTO_PATH = r_proto)
}
r_scripts <- list.files(r_dir, pattern = "\\.R$", full.names = TRUE)

for (script_path in r_scripts) {
  sys.source(script_path, envir = globalenv())
}

scipr_read_index <- function(path) {
  find_proto_path(project_root)
  RProtoBuf::read(RProtoBuf::P("scip.Index"), path)
}

scipr_document <- function(index, relative_path) {
  docs <- Filter(
    function(doc) identical(doc$relative_path, relative_path),
    index$documents
  )
  testthat::expect_length(docs, 1)
  docs[[1]]
}

scipr_symbols_by_id <- function(document) {
  stats::setNames(
    as.list(document$symbols),
    vapply(document$symbols, function(symbol) symbol$symbol, character(1))
  )
}

scipr_occurrences_for <- function(document, symbol, role = NULL) {
  matches <- Filter(
    function(occurrence) identical(occurrence$symbol, symbol),
    as.list(document$occurrences)
  )

  if (is.null(role)) {
    return(matches)
  }

  role_enum <- RProtoBuf::P("scip.SymbolRole")
  role_value <- switch(role,
    definition = as.integer(role_enum$Definition),
    reference = as.integer(role_enum$ReadAccess),
    testthat::fail(sprintf("Unknown SCIP occurrence role: %s", role))
  )
  Filter(
    function(occurrence) bitwAnd(as.integer(occurrence$symbol_roles), role_value) != 0L,
    matches
  )
}

scipr_symbol_kind <- function(kind) {
  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  as.integer(kind_enum[[kind]])
}

# Exercise the actual native backend, never a silent R fallback.
with_resolver_backend <- function(backend, code) {
  testthat::expect_true(requireNamespace("scipR", quietly = TRUE))
  env <- environment(resolve_symbols)
  original_available <- get("rcpp_available", envir = env)
  native_names <- ls(env, pattern = "_cpp$")
  original_native <- mget(native_names, envir = env)
  # Sourced Rcpp wrappers lack their namespace's registered native addresses.
  # Use the installed wrappers for native calls, retaining source R orchestration.
  for (name in native_names) {
    assign(name, get(name, envir = asNamespace("scipR")), envir = env)
  }
  original_resolve <- get("resolve_symbols_cpp", envir = env)
  state <- new.env(parent = emptyenv())
  state$cpp_success <- 0L
  assign("rcpp_available", function() identical(backend, "cpp"), envir = env)
  assign("resolve_symbols_cpp", function(...) {
    value <- original_resolve(...)
    state$cpp_success <- state$cpp_success + 1L
    value
  }, envir = env)
  on.exit({
    assign("rcpp_available", original_available, envir = env)
    for (name in native_names) assign(name, original_native[[name]], envir = env)
  }, add = TRUE)
  code()
  if (identical(backend, "cpp")) {
    testthat::expect_gt(state$cpp_success, 0L)
  } else {
    testthat::expect_equal(state$cpp_success, 0L)
  }
}

# Measure extraction after parsing, using paired CPU samples so scheduler
# pauses do not become extraction regressions. The caller owns its growth bound.
scipr_extraction_cpu_growth <- function(extract, input, baseline) {
  invisible(extract(baseline))
  invisible(extract(input))
  measure <- function(ast, batch) {
    timing <- system.time(for (i in seq_len(batch)) invisible(extract(ast)))
    unname(timing[["user.self"]] + timing[["sys.self"]])
  }
  # Resolve the CPU timer even if the extractor becomes substantially faster.
  batch <- 1L
  while (measure(baseline, batch) < 0.05 && batch < 1024L) batch <- batch * 2L
  baseline_cpu <- sample_cpu <- numeric(5L)
  for (i in seq_len(5L)) {
    if (i %% 2L == 1L) {
      baseline_cpu[i] <- measure(baseline, batch)
      sample_cpu[i] <- measure(input, batch)
    } else {
      sample_cpu[i] <- measure(input, batch)
      baseline_cpu[i] <- measure(baseline, batch)
    }
  }
  testthat::expect_true(all(baseline_cpu > 0), info = "CPU timer must resolve the calibrated batch")
  list(
    growth = stats::median(sample_cpu / baseline_cpu),
    sample_cpu = stats::median(sample_cpu) / batch,
    baseline_cpu = stats::median(baseline_cpu) / batch,
    batch = batch
  )
}
