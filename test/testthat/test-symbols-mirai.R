# Specs for R/symbols_mirai.R.
#
# Anti-regression cover for Architext-1nhe8: the parallel resolver
# (1) read mirai >= 2.0's status()$daemons, the dispatcher URL, as "1 daemon
# online", so wait_for_daemons() ran to its 30s timeout on every run, and
# (2) sent one task per file, each serializing the whole symbol table, which
# spent 44s of dispatch on a 595-file shard.

testthat::test_that("online_daemons_from_status reads connections from a dispatcher status", {
  dispatcher <- list(
    connections = 8L,
    daemons = "ipc:///tmp/83eeba5f584a24f79545666b",
    mirai = c(awaiting = 0L, executing = 0L, completed = 0L)
  )
  testthat::expect_identical(online_daemons_from_status(dispatcher), 8L)
  testthat::expect_identical(
    online_daemons_from_status(modifyList(dispatcher, list(connections = 0L))),
    0L
  )
})

testthat::test_that("online_daemons_from_status keeps the legacy status shapes", {
  testthat::expect_identical(online_daemons_from_status(NULL), 0L)
  testthat::expect_identical(online_daemons_from_status(list(daemons = NULL)), 0L)
  matrix_status <- list(daemons = cbind(online = c(1L, 1L, 0L), instance = 1:3))
  testthat::expect_identical(online_daemons_from_status(matrix_status), 2L)
  testthat::expect_identical(online_daemons_from_status(list(daemons = list(1, 2, 3))), 3L)
})

testthat::test_that("chunk_files covers every file once in a bounded number of chunks", {
  for (case in list(c(n = 595L, d = 8L), c(n = 88L, d = 2L), c(n = 3L, d = 4L), c(n = 1L, d = 1L))) {
    chunks <- chunk_files(case[["n"]], case[["d"]])
    testthat::expect_lte(length(chunks), case[["d"]] * 4L)
    testthat::expect_identical(sort(unlist(chunks)), seq_len(case[["n"]]))
    testthat::expect_true(all(lengths(chunks) > 0L))
  }
  testthat::expect_identical(chunk_files(0L, 4L), list())
  testthat::expect_length(chunk_files(10L, 0L), 4L)
})

testthat::test_that("unchunk_results restores file order", {
  chunks <- chunk_files(11L, 1L)
  results <- lapply(chunks, function(idx) as.list(sprintf("file-%d", idx)))
  testthat::expect_identical(unchunk_results(results, chunks), as.list(sprintf("file-%d", 1:11)))
})

testthat::test_that("parallel resolution matches sequential documents", {
  testthat::skip_on_cran()
  # Daemons load the installed scipR, not the sourced scripts.
  testthat::skip_if_not_installed("scipR")
  testthat::skip_if_not_installed("mirai")

  root <- tempfile("scipr-mirai-")
  dir.create(file.path(root, "R"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  for (i in 1:12) {
    callee <- if (i > 1) sprintf("  helper_%d(x) + ", i - 1) else "  "
    writeLines(c(
      sprintf("helper_%d <- function(x) {", i),
      sprintf("%sx * %d", callee, i),
      "}",
      sprintf("print.thing_%d <- function(x, ...) helper_%d(x$value)", i, i)
    ), file.path(root, "R", sprintf("file_%02d.R", i)))
  }

  run <- function(parallel) {
    output <- file.path(root, sprintf("index-%d.scip", parallel))
    messages <- character()
    withCallingHandlers(
      index_project(list(
        root = root, output = output, include = "R/**/*.R",
        namespace = NULL, `project-name` = NULL, cache = FALSE,
        parallel = parallel
      )),
      message = function(m) {
        messages <<- c(messages, conditionMessage(m))
        invokeRestart("muffleMessage")
      }
    )
    # A failed parallel path falls back to sequential and would match
    # trivially; the parity claim holds only if the daemons did the work.
    testthat::expect_false(any(grepl("Falling back|daemons connected", messages)))
    index <- scipr_read_index(output)
    docs <- as.list(index$documents)
    names(docs) <- vapply(docs, function(doc) doc$relative_path, character(1))
    lapply(docs[order(names(docs))], function(doc) doc$toString())
  }

  sequential <- run(0L)
  testthat::expect_length(sequential, 12L)
  testthat::expect_identical(run(2L), sequential)
})
