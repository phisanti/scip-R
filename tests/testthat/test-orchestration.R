# Specs for R/orchestration.R backend routing.
#
# Anti-regression cover: index_project() used to call
# serialize_documents_mirai() unguarded, so a mirai failure (daemons denied
# sockets in a sandboxed environment, mirai missing) killed the whole run even
# though index_documents_sequential() would have produced the same index.
# Tools that enable --parallel automatically on large projects lost the
# whole index, and anything reading it kept answering from a stale copy.

# Swap scipR namespace bindings for the duration of `expr`, restoring the
# originals on exit even if `expr` throws.
scipr_with_stubs <- function(stubs, expr) {
  do.call(testthat::local_mocked_bindings, c(stubs, list(.package = "scipR")))
  force(expr)
}

# cli::cli_alert_* signal conditions that inherit from "message". Collect them
# rather than letting them print, and return the value alongside.
scipr_collect_messages <- function(expr) {
  msgs <- character()
  value <- withCallingHandlers(
    force(expr),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  list(value = value, messages = paste(msgs, collapse = "\n"))
}

scipr_stub_cfg <- function(parallel) {
  list(
    root = "/tmp/scipr-orchestration-spec",
    output = "/tmp/scipr-orchestration-spec/index.scip",
    `project-name` = NULL,
    cache = FALSE,
    parallel = parallel
  )
}

scipr_stub_workspace <- function() {
  list(metadata = list(project_name = "orchestration-spec"))
}

scipr_stub_files <- function() {
  list(
    list(path = "/tmp/scipr-orchestration-spec/R/a.R", relative_path = "R/a.R"),
    list(path = "/tmp/scipr-orchestration-spec/R/b.R", relative_path = "R/b.R")
  )
}

# Base stubs that keep index_project off the filesystem. Individual tests
# override the two backend functions.
scipr_base_stubs <- function() {
  list(
    analyze_workspace = function(root) scipr_stub_workspace(),
    discover_files = function(workspace, cache_enabled = FALSE, include_patterns = NULL) {
      list(files = scipr_stub_files())
    }
  )
}

test_that("index_project falls back to sequential when the mirai path aborts", {
  stubs <- c(
    scipr_base_stubs(),
    list(
      serialize_documents_mirai = function(...) {
        stop("mirai daemons failed to come online within 30s.")
      },
      index_documents_sequential = function(files, workspace, output_path) "SEQUENTIAL"
    )
  )

  result <- scipr_with_stubs(stubs, {
    scipr_collect_messages(index_project(scipr_stub_cfg(parallel = 4L)))
  })

  expect_identical(result$value, "SEQUENTIAL")
})

test_that("the fallback warns instead of failing silently", {
  stubs <- c(
    scipr_base_stubs(),
    list(
      serialize_documents_mirai = function(...) {
        stop("mirai daemons failed to come online within 30s.")
      },
      index_documents_sequential = function(files, workspace, output_path) "SEQUENTIAL"
    )
  )

  result <- scipr_with_stubs(stubs, {
    scipr_collect_messages(index_project(scipr_stub_cfg(parallel = 4L)))
  })

  expect_match(result$messages, "Parallel resolution failed")
  expect_match(result$messages, "mirai daemons failed to come online")
  expect_match(result$messages, "Falling back to sequential")
})

test_that("the sequential fallback receives the parallel path's inputs", {
  captured <- NULL

  stubs <- c(
    scipr_base_stubs(),
    list(
      serialize_documents_mirai = function(...) stop("no daemons"),
      index_documents_sequential = function(files, workspace, output_path) {
        captured <<- list(files = files, workspace = workspace, output_path = output_path)
        "SEQUENTIAL"
      }
    )
  )

  scipr_with_stubs(stubs, {
    scipr_collect_messages(index_project(scipr_stub_cfg(parallel = 2L)))
  })

  expect_false(is.null(captured))
  expect_identical(captured$files, scipr_stub_files())
  expect_identical(captured$workspace, scipr_stub_workspace())
  expect_identical(captured$output_path, scipr_stub_cfg(parallel = 2L)$output)
})

test_that("a successful parallel run never touches the sequential path", {
  sequential_calls <- 0L

  stubs <- c(
    scipr_base_stubs(),
    list(
      serialize_documents_mirai = function(files, workspace, output_path, daemons) "PARALLEL",
      index_documents_sequential = function(files, workspace, output_path) {
        sequential_calls <<- sequential_calls + 1L
        "SEQUENTIAL"
      }
    )
  )

  result <- scipr_with_stubs(stubs, {
    scipr_collect_messages(index_project(scipr_stub_cfg(parallel = 4L)))
  })

  expect_identical(result$value, "PARALLEL")
  expect_identical(sequential_calls, 0L)
})

scipr_no_file_stubs <- function() {
  list(
    analyze_workspace = function(root) {
      list(metadata = list(project_name = "orchestration-spec", project_root = root))
    },
    discover_files = function(workspace, cache_enabled = FALSE, include_patterns = NULL) {
      list(files = list())
    },
    index_documents_sequential = function(...) stop("must not index zero files")
  )
}

test_that("index_project aborts when include patterns match no files", {
  cfg <- scipr_stub_cfg(parallel = 0L)
  cfg$include <- "src/**/*.R"
  scipr_with_stubs(scipr_no_file_stubs(), {
    expect_error(index_project(cfg), "No R files to index")
  })
})

test_that("the zero-file abort names every include pattern", {
  cfg <- scipr_stub_cfg(parallel = 0L)
  cfg$include <- c("src/**/*.R", "app/*.R")
  err <- scipr_with_stubs(scipr_no_file_stubs(), {
    tryCatch(index_project(cfg), error = function(e) conditionMessage(e))
  })
  expect_match(err, "src/**/*.R", fixed = TRUE)
  expect_match(err, "app/*.R", fixed = TRUE)
})

test_that("scip-r CLI aborts on an unmatched --include and writes no index", {
  root <- tempfile("scipr_unmatched_")
  dir.create(file.path(root, "R"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("f <- function() 1", file.path(root, "R", "f.R"))
  output <- file.path(root, "index.scip")
  expect_error(
    scipr_run_cli(c("--root", root, "--output", output, "--include", "src/**/*.R")),
    "No R files to index"
  )
  expect_false(file.exists(output))
})

# --namespace was removed (it never changed the index); passing it must fail.
test_that("scip-r CLI rejects the removed --namespace flag", {
  root <- tempfile("scipr_namespace_")
  dir.create(file.path(root, "R"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  writeLines("f <- function() 1", file.path(root, "R", "f.R"))
  output <- file.path(root, "index.scip")
  expect_error(scipr_run_cli(c(
    "--root", root, "--output", output, "--include", "R/**/*.R", "--namespace", "zz"
  )))
  expect_false(file.exists(output))
})

test_that("index_project no longer forwards a namespace to the workspace", {
  seen <- NULL
  stubs <- c(
    list(analyze_workspace = function(...) {
      seen <<- list(...)
      scipr_stub_workspace()
    }),
    scipr_base_stubs()["discover_files"],
    list(index_documents_sequential = function(...) "SEQUENTIAL")
  )
  cfg <- scipr_stub_cfg(parallel = 0L)
  scipr_collect_messages(scipr_with_stubs(stubs, index_project(cfg)))
  expect_length(seen, 1L)
})
