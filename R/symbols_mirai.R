#' @include benchmark.R
#' @include parser.R
#' @include symbol-table.R
#' @include symbols.R
#' @include occurrences.R
#' @include diagnostics.R
NULL

# This script introduces mirai-powered parallel symbol resolution for documents.
# It evaluates parsing, symbol extraction, and document construction concurrently.
#
# Architecture for cross-file resolution:
# 1. Parse all files serially (fast, I/O bound)
# 2. Build symbol table serially (requires all parsed files)
# 3. Resolve symbols in parallel with shared symbol table

#' Resolve documents in parallel using mirai daemons.
#'
#' Uses a three-phase approach for cross-file reference resolution:
#' 1. Parse all files (serial - I/O bound, fast)
#' 2. Build symbol table (serial - needs all files)
#' 3. Resolve and serialize in parallel (CPU bound)
#'
#' @param files List of file descriptors from `discover_files()`.
#' @param workspace Workspace context obtained via `analyze_workspace()`.
#' @param daemons Number of mirai daemons to launch (defaults to physical cores).
#' @return A list of lists, each containing `document` and `diagnostics` entries.
#' @keywords internal
#' @examples
#' \dontrun{
#' docs <- resolve_documents_mirai(plan$files, workspace, daemons = 4)
#' }
resolve_documents_mirai <- function(files, workspace, daemons = parallel::detectCores(logical = FALSE)) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg mirai} is required for parallel resolution.",
      "i" = "Install it via: {.code install.packages('mirai')}"
    ))
  }

  # Phase 1: Parse all files serially (I/O bound, very fast)
  cli::cli_alert_info("Phase 1: Parsing {.val {length(files)}} files...")
  t_phase1 <- bench_start("mirai_phase1_parsing", list(file_count = length(files)))
  parsed_files <- list()
  file_infos <- list()

  for (file_info in files) {
    needs_index <- file_info$needs_index
    if (!is.null(needs_index) && isFALSE(needs_index)) {
      next
    }

    t_parse <- bench_start("parse_file", list(file = file_info$relative_path %||% basename(file_info$path)))
    ast <- tryCatch(
      parse_file(file_info),
      error = function(e) {
        cli::cli_alert_warning(
          "Parse failed for {.path {file_info$relative_path %||% file_info$path}}: {conditionMessage(e)}"
        )
        NULL
      }
    )
    bench_end(t_parse)

    if (!is.null(ast)) {
      parsed_files[[length(parsed_files) + 1]] <- ast
      file_infos[[length(file_infos) + 1]] <- file_info
    }
  }
  bench_end(t_phase1, list(parsed_count = length(parsed_files)))

  # Phase 2: Build symbol table (requires all parsed files)
  cli::cli_alert_info("Phase 2: Building symbol table...")
  t_phase2 <- bench_start("mirai_phase2_symbol_table", list(file_count = length(parsed_files)))
  symbol_table <- build_symbol_table(parsed_files, workspace)
  bench_end(t_phase2, list(definition_count = length(symbol_table$definitions)))
  cli::cli_alert_success(
    "Found {.val {length(symbol_table$definitions)}} package-level definitions"
  )

  # Phase 3: Resolve symbols in parallel
  cli::cli_alert_info("Phase 3: Resolving symbols in parallel with {.val {daemons}} workers...")
  t_phase3 <- bench_start("mirai_phase3_parallel_resolve", list(file_count = length(parsed_files), workers = daemons))

  daemons <- max(1L, daemons)
  t_setup <- bench_start("mirai_daemon_setup", list(workers = daemons))
  mirai::daemons(daemons)
  bench_end(t_setup)
  on.exit(mirai::daemons(0), add = TRUE)

  # Wait briefly for daemons to actually come online. mirai::daemons(N) returns
  # immediately after asking the dispatcher to launch N workers; if the workers
  # never connect (segfault during init, missing libs in --vanilla env, etc.)
  # later collect_mirai() calls block forever. Fail fast instead.
  daemons_online <- wait_for_daemons(daemons, timeout_secs = 30)
  if (daemons_online < 1L) {
    cli::cli_abort(c(
      "mirai daemons failed to come online within 30s.",
      "i" = "Expected {.val {daemons}} workers, observed {.val {daemons_online}} online."
    ))
  }
  if (daemons_online < daemons) {
    cli::cli_alert_warning(
      "Only {.val {daemons_online}} of {.val {daemons}} mirai daemons connected; continuing with reduced parallelism."
    )
  }

  # Strip externalptr-bearing fields before serializing to daemons. mirai
  # transmits args via R serialize(): plain externalptrs serialize to a
  # placeholder that segfaults when dereferenced in the receiving process,
  # killing the daemon silently. tree_index$cpp_index (Rcpp XPtr from
  # build_tree_index_cpp) and every entry of symbol_table$tree_indices carry
  # such pointers. Rebuild tree_index inside the daemon from ast$parse_data;
  # daemons do not consult symbol_table$tree_indices.
  symbol_table_for_dispatch <- symbol_table
  symbol_table_for_dispatch$tree_indices <- NULL

  t_dispatch <- bench_start("mirai_task_dispatch", list(task_count = length(parsed_files)))
  tasks <- lapply(seq_along(parsed_files), function(i) {
    ast <- parsed_files[[i]]
    file_info <- file_infos[[i]]

    mirai::mirai(
      {
        library(scipR)
        ns <- asNamespace("scipR")
        # Rebuild tree_index locally so cpp_index references daemon-side memory
        tree_index <- get("build_tree_index", envir = ns)(ast$parse_data)
        analysis <- get("resolve_symbols", envir = ns)(
          ast,
          workspace,
          track_externals = TRUE,
          symbol_table = symbol_table,
          tree_index = tree_index
        )
        occurrences <- get("map_occurrences", envir = ns)(
          analysis, ast, tree_index = tree_index
        )
        document <- get("build_document", envir = ns)(
          file_info, occurrences, analysis
        )
        diagnostics <- get("collect_diagnostics", envir = ns)(
          file_info, ast, analysis
        )
        list(document = document, diagnostics = diagnostics, external_refs = analysis$external_refs)
      },
      ast = ast,
      file_info = file_info,
      workspace = workspace,
      symbol_table = symbol_table_for_dispatch
    )
  })
  bench_end(t_dispatch)

  # Collect with a stall watchdog. mirai::collect_mirai() has no timeout, so a
  # silently dead daemon (e.g. segfault from an externalptr it shouldn't have
  # received) leaves us blocked forever. Poll until done or until no new task
  # has completed in `stall_timeout_secs`, then abort with diagnostics.
  t_collect <- bench_start("mirai_task_collect", list(task_count = length(tasks)))
  results <- collect_with_watchdog(tasks, expected_daemons = daemons, stall_timeout_secs = 120)
  bench_end(t_collect)

  mirai_errors <- vapply(results, inherits, logical(1), what = "miraiError")
  if (any(mirai_errors)) {
    first_error <- results[[which(mirai_errors)[1]]]
    cli::cli_abort(c(
      "Parallel symbol resolution failed in {.val {sum(mirai_errors)}} worker(s).",
      "i" = conditionMessage(first_error)
    ))
  }

  bench_end(t_phase3)
  results
}

# Poll mirai::status() until at least one daemon is online or the timeout
# elapses. Returns the number of online daemons observed.
wait_for_daemons <- function(expected, timeout_secs = 30) {
  deadline <- Sys.time() + timeout_secs
  online <- 0L
  repeat {
    online <- mirai_online_daemons()
    if (online >= expected) {
      return(online)
    }
    if (Sys.time() >= deadline) {
      return(online)
    }
    Sys.sleep(0.2)
  }
}

# Return the number of daemons currently online, robust to mirai status()
# returning either a matrix (dispatcher mode) or a list across versions.
mirai_online_daemons <- function() {
  st <- tryCatch(mirai::status(), error = function(e) NULL)
  if (is.null(st) || is.null(st$daemons)) {
    return(0L)
  }
  d <- st$daemons
  if (is.matrix(d)) {
    if ("online" %in% colnames(d)) {
      return(as.integer(sum(d[, "online"] > 0)))
    }
    return(nrow(d))
  }
  if (is.character(d)) {
    return(1L)
  } # single dispatcher URL, daemons unknown
  if (is.list(d)) {
    return(length(d))
  }
  0L
}

# Collect mirai task results with a no-progress watchdog. Aborts (rather than
# blocking forever) if no task completes within `stall_timeout_secs`, with a
# diagnostic that distinguishes "all daemons died" from "tasks just slow."
collect_with_watchdog <- function(tasks, expected_daemons, stall_timeout_secs = 120) {
  n <- length(tasks)
  if (n == 0L) {
    return(list())
  }

  last_completed <- 0L
  last_progress_at <- Sys.time()

  repeat {
    pending <- vapply(tasks, mirai::unresolved, logical(1))
    completed <- n - sum(pending)

    if (completed > last_completed) {
      last_completed <- completed
      last_progress_at <- Sys.time()
    }

    if (!any(pending)) break

    if (as.numeric(difftime(Sys.time(), last_progress_at, units = "secs")) > stall_timeout_secs) {
      online <- mirai_online_daemons()
      cli::cli_abort(c(
        "Parallel symbol resolution stalled: no task completed in {.val {stall_timeout_secs}}s.",
        "i" = "Online daemons: {.val {online}} of {.val {expected_daemons}}.",
        "i" = "Completed: {.val {completed}} of {.val {n}} tasks.",
        "i" = paste(
          "This usually means a daemon segfaulted on a task arg",
          "(often a stale externalptr) and no replacement is coming."
        ),
        "x" = "Retry with sequential mode (--parallel=0) to confirm."
      ))
    }

    Sys.sleep(0.5)
  }

  lapply(tasks, function(task) task$data)
}
