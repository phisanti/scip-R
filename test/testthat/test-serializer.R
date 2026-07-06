# This test covers serializer lifecycle from initialization to closure.
# It confirms document accumulation and output persistence for downstream use.

testthat::test_that("serializer writes readable SCIP protobuf artifact", {
  tmp <- tempfile(fileext = ".scip")
  ws <- analyze_workspace(tempdir())
  ser <- init_serializer(tmp, ws)
  write_document(ser, list(relative_path = "script.R", language = "R"))
  write_externals(ser, list())
  path <- close_serializer(ser)
  testthat::expect_true(file.exists(path))

  index <- RProtoBuf::read(RProtoBuf::P("scip.Index"), path)
  testthat::expect_equal(index$metadata$tool_info$name, "scip-r")
  testthat::expect_equal(
    unname(index$metadata$tool_info$version),
    unname(scip_r_version())
  )
  testthat::expect_length(index$documents, 1)
  testthat::expect_equal(index$documents[[1]]$relative_path, "script.R")
  testthat::expect_equal(index$documents[[1]]$language, "R")
})


test_that("scip proto is loadable from install or fallback path", {
  project_root <- normalizePath(
    file.path(testthat::test_path("."), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  installed_path <- system.file("scip.proto", package = "scipR")
  fallback_raw <- file.path(project_root, "..", "inst", "scip.proto")
  fallback_path <- if (file.exists(fallback_raw)) {
    normalizePath(fallback_raw, winslash = "/", mustWork = TRUE)
  } else {
    ""
  }

  original_proto <- Sys.getenv("SCIP_PROTO_PATH", unset = NA_character_)
  on.exit({
    if (is.na(original_proto)) {
      Sys.unsetenv("SCIP_PROTO_PATH")
    } else {
      Sys.setenv(SCIP_PROTO_PATH = original_proto)
    }
  }, add = TRUE)

  proto_candidate <- ""
  if (nzchar(installed_path) && file.exists(installed_path)) {
    proto_candidate <- normalizePath(installed_path, winslash = "/", mustWork = TRUE)
    Sys.unsetenv("SCIP_PROTO_PATH")
  } else if (nzchar(fallback_path)) {
    Sys.setenv(SCIP_PROTO_PATH = fallback_path)
    proto_candidate <- fallback_path
  } else {
    fail("scip.proto not found in installed package or ../inst fallback")
  }

  located <- find_proto_path(project_root)
  expect_equal(located, proto_candidate)
  expect_true(proto_loaded(located))

  descriptor <- RProtoBuf::P("scip.Index")
  expect_s4_class(descriptor, "Descriptor")
})
