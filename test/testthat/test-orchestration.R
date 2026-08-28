# Specs for R/orchestration.R backend routing.
#
# Anti-regression cover for Architext-0kdq: index_project() used to call
# serialize_documents_mirai() unguarded, so a mirai failure (daemons denied
# sockets in a sandboxed environment, mirai missing) killed the whole run even
# though index_documents_sequential() would have produced the same index.
# Callers cannot dodge this — semquery appends --parallel automatically at
# >= 50 R files with no opt-out — so the abort dropped the R SCIP shard and
# every downstream answer came from a stale index.

# Swap bindings in globalenv (where helper-scip-r.R sources the modules) for the
# duration of `expr`, restoring the originals on exit even if `expr` throws.
scipr_with_stubs <- function(stubs, expr) {
  env <- globalenv()
  names_to_swap <- names(stubs)
  originals <- stats::setNames(vector("list", length(names_to_swap)), names_to_swap)
  existed <- vapply(names_to_swap, exists, logical(1), envir = env, inherits = FALSE)

  for (nm in names_to_swap[existed]) {
    originals[[nm]] <- get(nm, envir = env, inherits = FALSE)
  }
  on.exit({
    for (nm in names_to_swap) {
      if (existed[[nm]]) {
        assign(nm, originals[[nm]], envir = env)
      } else {
        rm(list = nm, envir = env)
      }
    }
  }, add = TRUE)

  for (nm in names_to_swap) {
    assign(nm, stubs[[nm]], envir = env)
  }
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
    namespace = NULL,
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
    analyze_workspace = function(root, namespace = NULL) scipr_stub_workspace(),
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
