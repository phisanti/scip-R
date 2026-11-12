# Shared fixtures for the scipR tests. Tests run inside the package namespace
# (testthat::test_check() or testthat::test_local()), so internals are visible.

# Source checkout root. Under R CMD check the tests run from a copy, where
# source-only files (R/, .example/) are absent; tests needing them skip.
project_root <- normalizePath(file.path(testthat::test_path("."), "..", ".."), winslash = "/", mustWork = TRUE)

# Load the scip.proto descriptors once, so a test file that builds protobuf
# messages passes on its own and not only after a file that wrote an index.
invisible(find_proto_path(project_root))

skip_if_no_source_file <- function(path) {
  testthat::skip_if_not(file.exists(path), paste("source checkout only:", basename(path)))
}

# Run the scip-r command line in-process through Rapp's argument parser.
scipr_run_cli <- function(args) {
  app <- system.file("exec", "scip-r.R", package = "scipR")
  suppressMessages(Rapp::run(app, args))
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
  original_resolve <- resolve_symbols_cpp
  state <- new.env(parent = emptyenv())
  state$cpp_success <- 0L
  testthat::local_mocked_bindings(
    rcpp_available = function() identical(backend, "cpp"),
    resolve_symbols_cpp = function(...) {
      value <- original_resolve(...)
      state$cpp_success <- state$cpp_success + 1L
      value
    }
  )
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
