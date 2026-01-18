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

  t_dispatch <- bench_start("mirai_task_dispatch", list(task_count = length(parsed_files)))
  tasks <- lapply(seq_along(parsed_files), function(i) {
    ast <- parsed_files[[i]]
    file_info <- file_infos[[i]]

    mirai::mirai(
      {
        library(scipR)
        analysis <- resolve_symbols(ast, workspace, track_externals = TRUE, symbol_table = symbol_table)
        occurrences <- map_occurrences(analysis, ast)
        document <- build_document(file_info, occurrences, analysis)
        diagnostics <- collect_diagnostics(file_info, ast, analysis)
        list(document = document, diagnostics = diagnostics, external_refs = analysis$external_refs)
      },
      ast = ast,
      file_info = file_info,
      workspace = workspace,
      symbol_table = symbol_table
    )
  })
  bench_end(t_dispatch)

  t_collect <- bench_start("mirai_task_collect", list(task_count = length(tasks)))
  results <- lapply(tasks, function(task) mirai::collect_mirai(task))
  bench_end(t_collect)

  bench_end(t_phase3)
  results
}
