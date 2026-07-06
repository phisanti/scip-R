#' @include workspace.R
#' @include files.R
#' @include benchmark.R
#' @include parser.R
#' @include symbol-table.R
#' @include symbols.R
#' @include occurrences.R
#' @include diagnostics.R
#' @include serializer.R
NULL

# Orchestration layer for scip-r indexing pipeline.
# This module coordinates the end-to-end indexing workflow, from workspace
# analysis through file discovery, parsing, symbol resolution, and SCIP
# serialization. It provides both sequential and parallel execution paths.
#
# Key architectural change (cross-file references):
# The pipeline now includes a symbol table building phase between parsing
# and symbol resolution. This enables cross-file reference tracking by
# collecting all package-level definitions before resolving references.

#' Index a project and generate a SCIP file.
#'
#' Orchestrates the complete indexing pipeline for an R project:
#' 1. Analyzes the workspace to identify packages and structure
#' 2. Discovers indexable R files respecting ignore patterns
#' 3. Parses source files sequentially (benchmarks show parallel parsing adds overhead)
#' 4. Resolves symbols and generates SCIP documents (uses mirai if parallel > 0)
#' 5. Writes the final .scip index file
#'
#' @param cfg Configuration list with the following fields:
#'   \describe{
#'     \item{root}{Project root directory (normalized absolute path)}
#'     \item{output}{Destination .scip file path (normalized absolute path)}
#'     \item{namespace}{Optional namespace prefix for local symbols (character or NULL)}
#'     \item{project-name}{Optional override for project name in metadata (character or NULL)}
#'     \item{cache}{Enable cache hints for file discovery (logical)}
#'     \item{parallel}{Number of mirai workers for symbol resolution (integer, 0 = sequential)}
#'   }
#'
#' @return Invisibly returns the output `.scip` path on success.
#' @export
#' @examples
#' \dontrun{
#' # Sequential indexing
#' cfg <- list(
#'   root = "/path/to/project",
#'   output = "index.scip",
#'   namespace = NULL,
#'   `project-name` = NULL,
#'   cache = FALSE,
#'   parallel = 0L
#' )
#' index_project(cfg)
#'
#' # Parallel indexing with 4 workers
#' cfg$parallel <- 4L
#' index_project(cfg)
#' }
index_project <- function(cfg) {
  t_total <- bench_start("index_project_total", list(root = cfg$root))

  # Analyze workspace structure
  t_ws <- bench_start("analyze_workspace", list(root = cfg$root))
  workspace <- analyze_workspace(cfg$root, cfg$namespace)
  bench_end(t_ws)

  # Override project name if specified
  if (!is.null(cfg$`project-name`) && nzchar(cfg$`project-name`)) {
    workspace$metadata$project_name <- cfg$`project-name`
  }

  # Discover files to index
  t_disc <- bench_start("discover_files", list(cache = cfg$cache))
  plan <- discover_files(
    workspace,
    cache_enabled = cfg$cache,
    include_patterns = cfg$include
  )
  bench_end(t_disc, list(file_count = length(plan$files)))

  # Determine execution mode
  effective_parallel <- cfg$parallel %||% 0L

  # Route to appropriate backend
  result <- if (effective_parallel > 0) {
    # Parallel mode: use mirai for symbol resolution and serialization
    # Benchmarks show ~40-50x speedup on larger repos (40+ files)
    serialize_documents_mirai(
      plan$files,
      workspace,
      cfg$output,
      daemons = effective_parallel
    )
  } else {
    # Sequential mode: single-threaded processing
    # Faster for small repos (<10 files); mirai overhead dominates
    index_documents_sequential(plan$files, workspace, cfg$output)
  }

  bench_end(t_total, list(file_count = length(plan$files), parallel = effective_parallel))
  bench_report_counts()

  invisible(result)
}

#' Sequential indexing fallback when parallel mode is disabled.
#'
#' Processes files in three phases:
#' 1. Parse all files to collect ASTs
#' 2. Build package-wide symbol table for cross-file resolution
#' 3. Resolve symbols and serialize documents
#'
#' This enables cross-file reference tracking: when file A calls a function
#' defined in file B, the reference points to the same symbol ID as the definition.
#'
#' @param files File descriptor list from `discover_files()`.
#' @param workspace Workspace metadata list from `analyze_workspace()`.
#' @param output_path Destination `.scip` file path.
#' @return Output path (invisibly).
#' @keywords internal
index_documents_sequential <- function(files, workspace, output_path) {
  t_seq <- bench_start("sequential_indexing", list(file_count = length(files)))

  serializer <- init_serializer(output_path, workspace)
  on.exit(if (!is.null(serializer)) close_serializer(serializer), add = TRUE)

  # Phase 1: Parse all files
  cli::cli_alert_info("Phase 1: Parsing {.val {length(files)}} files...")
  t_phase1 <- bench_start("phase1_parsing", list(file_count = length(files)))
  parsed_files <- list()
  file_infos <- list()

  for (file_info in files) {
    # Skip files marked as not needing indexing
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

  # Phase 2: Build symbol table for cross-file resolution
  cli::cli_alert_info("Phase 2: Building symbol table...")
  t_phase2 <- bench_start("phase2_symbol_table", list(file_count = length(parsed_files)))
  symbol_table <- build_symbol_table(parsed_files, workspace)
  bench_end(t_phase2, list(definition_count = length(symbol_table$definitions)))
  cli::cli_alert_success(
    "Found {.val {length(symbol_table$definitions)}} package-level definitions"
  )

  # Phase 3: Resolve symbols and serialize
  # OPTIMIZATION: Reuse tree indices from Phase 2
  cli::cli_alert_info("Phase 3: Resolving symbols and serializing...")
  t_phase3 <- bench_start("phase3_resolve_serialize", list(file_count = length(parsed_files)))
  all_external_refs <- list()

  for (i in seq_along(parsed_files)) {
    ast <- parsed_files[[i]]
    file_info <- file_infos[[i]]
    relative_path <- file_info$relative_path %||% basename(file_info$path)

    # Get cached tree_index from Phase 2 if available
    tree_index <- symbol_table$tree_indices[[relative_path]]

    t_proc <- bench_start("phase3_process_file", list(file = relative_path))
    result <- tryCatch(
      process_single_file_with_table(file_info, ast, workspace, symbol_table, tree_index = tree_index),
      error = function(e) {
        cli::cli_alert_warning(
          "Resolution failed for {.path {file_info$relative_path %||% file_info$path}}: {conditionMessage(e)}"
        )
        NULL
      }
    )
    bench_end(t_proc)

    if (is.null(result)) {
      next
    }

    # Write document and accumulate external references
    t_write <- bench_start("phase3_write_document")
    write_document(serializer, result$document, diagnostics = result$diagnostics)
    bench_end(t_write)

    if (length(result$external_refs) > 0) {
      all_external_refs <- merge_external_refs(all_external_refs, result$external_refs)
    }
  }
  bench_end(t_phase3)

  # Write accumulated external symbols
  t_ext <- bench_start("write_externals", list(package_count = length(all_external_refs)))
  write_externals(serializer, all_external_refs)
  bench_end(t_ext)

  result <- close_serializer(serializer)
  serializer <- NULL
  bench_end(t_seq)
  invisible(result)
}

#' Process a single file through the indexing pipeline.
#'
#' Coordinates the parse → resolve → occurrences → document workflow
#' for a single R source file. This version parses the file and does not
#' use cross-file symbol resolution (kept for backwards compatibility).
#'
#' @param file_info File descriptor list containing path, relative_path, etc.
#' @param workspace Workspace metadata list.
#' @return List with components:
#'   \describe{
#'     \item{document}{SCIP Document message}
#'     \item{diagnostics}{SCIP Diagnostic messages}
#'     \item{external_refs}{Symbols from external packages}
#'   }
#' @keywords internal
process_single_file <- function(file_info, workspace) {
  # Parse source file into AST
  ast <- parse_file(file_info)

  # Resolve symbols and track external package references (no cross-file resolution)
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE, symbol_table = NULL)

  # Map symbols to occurrence positions
  occurrences <- map_occurrences(analysis, ast)

  # Build SCIP document message
  document <- build_document(file_info, occurrences, analysis)

  # Collect any diagnostics (parse errors, resolution warnings, etc.)
  diagnostics <- collect_diagnostics(file_info, ast, analysis)

  list(
    document = document,
    diagnostics = diagnostics,
    external_refs = analysis$external_refs
  )
}

#' Process a single file with a pre-built symbol table.
#'
#' This version takes an already-parsed AST and uses the package-wide symbol
#' table for cross-file reference resolution. When file A calls a function
#' defined in file B, the reference will point to the same symbol ID as
#' the definition in file B.
#'
#' @param file_info File descriptor list containing path, relative_path, etc.
#' @param ast Pre-parsed AST module from parse_file().
#' @param workspace Workspace metadata list.
#' @param symbol_table Symbol table from build_symbol_table().
#' @param tree_index Optional tree index from build_tree_index() for O(1) lookups.
#' @return List with components:
#'   \describe{
#'     \item{document}{SCIP Document message}
#'     \item{diagnostics}{SCIP Diagnostic messages}
#'     \item{external_refs}{Symbols from external packages}
#'   }
#' @keywords internal
process_single_file_with_table <- function(file_info, ast, workspace, symbol_table, tree_index = NULL) {
  # OPTIMIZATION: Build tree_index once and share between resolve_symbols and map_occurrences
  if (is.null(tree_index)) {
    tree_index <- build_tree_index(ast$parse_data)
  }

  # Resolve symbols with cross-file resolution via symbol table
  t_resolve <- bench_start("phase3_resolve_symbols", list(file = file_info$relative_path %||% basename(file_info$path)))
  analysis <- resolve_symbols(
    ast,
    workspace,
    track_externals = TRUE,
    symbol_table = symbol_table,
    tree_index = tree_index
  )
  bench_end(t_resolve, list(symbol_count = length(analysis$symbols), occurrence_count = length(analysis$occurrences)))

  # Map symbols to occurrence positions (reuse tree_index)
  t_map <- bench_start("phase3_map_occurrences")
  occurrences <- map_occurrences(analysis, ast, tree_index = tree_index)
  bench_end(t_map)

  # Build SCIP document message
  t_doc <- bench_start("phase3_build_document")
  document <- build_document(file_info, occurrences, analysis)
  bench_end(t_doc)

  # Collect any diagnostics (parse errors, resolution warnings, etc.)
  t_diag <- bench_start("phase3_collect_diagnostics")
  diagnostics <- collect_diagnostics(file_info, ast, analysis)
  bench_end(t_diag)

  list(
    document = document,
    diagnostics = diagnostics,
    external_refs = analysis$external_refs
  )
}

#' Validate and normalize CLI arguments.
#'
#' Ensures the parallel worker count is:
#' - Non-negative
#' - Capped at physical CPU core count to avoid oversubscription
#' - Converted to integer
#'
#' @param parallel_value Parsed integer from CLI or config.
#' @return Integer number of workers to request (0 for sequential mode).
#' @export
#' @examples
#' \dontrun{
#' validate_args(0) # 0L (sequential)
#' validate_args(4) # 4L (or fewer if only 2 cores available)
#' validate_args(-1) # Error
#' validate_args(NULL) # 0L (default sequential)
#' }
validate_args <- function(parallel_value) {
  # Handle NULL/NA as sequential mode
  if (is.null(parallel_value) || is.na(parallel_value)) {
    return(0L)
  }

  # Validate non-negative
  if (parallel_value < 0) {
    cli::cli_abort("{.arg --parallel} must be >= 0")
  }

  # Sequential mode
  if (parallel_value == 0) {
    return(0L)
  }

  # Cap at physical core count to avoid oversubscription
  cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores)) {
    # Can't detect cores; trust user input
    return(as.integer(parallel_value))
  }

  as.integer(min(parallel_value, cores))
}
