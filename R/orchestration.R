#' @include workspace.R
#' @include files.R
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
  # Analyze workspace structure
  workspace <- analyze_workspace(cfg$root, cfg$namespace)

  # Override project name if specified
  if (!is.null(cfg$`project-name`) && nzchar(cfg$`project-name`)) {
    workspace$metadata$project_name <- cfg$`project-name`
  }

  # Discover files to index
  plan <- discover_files(workspace, cache_enabled = cfg$cache)

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
  serializer <- init_serializer(output_path, workspace)
  on.exit(close_serializer(serializer), add = TRUE)

  # Phase 1: Parse all files
  cli::cli_alert_info("Phase 1: Parsing {.val {length(files)}} files...")
  parsed_files <- list()
  file_infos <- list()

  for (file_info in files) {
    # Skip files marked as not needing indexing
    needs_index <- file_info$needs_index
    if (!is.null(needs_index) && isFALSE(needs_index)) {
      next
    }

    ast <- tryCatch(
      parse_file(file_info),
      error = function(e) {
        cli::cli_alert_warning(
          "Parse failed for {.path {file_info$relative_path %||% file_info$path}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (!is.null(ast)) {
      parsed_files[[length(parsed_files) + 1]] <- ast
      file_infos[[length(file_infos) + 1]] <- file_info
    }
  }

  # Phase 2: Build symbol table for cross-file resolution
  cli::cli_alert_info("Phase 2: Building symbol table...")
  symbol_table <- build_symbol_table(parsed_files, workspace)
  cli::cli_alert_success(
    "Found {.val {length(symbol_table$definitions)}} package-level definitions"
  )

  # Phase 3: Resolve symbols and serialize
  cli::cli_alert_info("Phase 3: Resolving symbols and serializing...")
  all_external_refs <- list()

  for (i in seq_along(parsed_files)) {
    ast <- parsed_files[[i]]
    file_info <- file_infos[[i]]

    result <- tryCatch(
      process_single_file_with_table(file_info, ast, workspace, symbol_table),
      error = function(e) {
        cli::cli_alert_warning(
          "Resolution failed for {.path {file_info$relative_path %||% file_info$path}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (is.null(result)) {
      next
    }

    # Write document and accumulate external references
    write_document(serializer, result$document, diagnostics = result$diagnostics)

    if (length(result$external_refs) > 0) {
      all_external_refs <- merge_external_refs(all_external_refs, result$external_refs)
    }
  }

  # Write accumulated external symbols
  write_externals(serializer, all_external_refs)
  result <- close_serializer(serializer)
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
#' @return List with components:
#'   \describe{
#'     \item{document}{SCIP Document message}
#'     \item{diagnostics}{SCIP Diagnostic messages}
#'     \item{external_refs}{Symbols from external packages}
#'   }
#' @keywords internal
process_single_file_with_table <- function(file_info, ast, workspace, symbol_table) {
  # Resolve symbols with cross-file resolution via symbol table
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE, symbol_table = symbol_table)

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

#' Validate and normalize the --parallel argument.
#'
#' Ensures the parallel worker count is:
#' - Non-negative
#' - Capped at physical CPU core count to avoid oversubscription
#' - Converted to integer
#'
#' @param parallel_value Parsed integer from CLI or config.
#' @return Integer number of workers to request (0 for sequential mode).
#' @keywords internal
#' @examples
#' \dontrun{
#' validate_parallel_arg(0)   # 0L (sequential)
#' validate_parallel_arg(4)   # 4L (or fewer if only 2 cores available)
#' validate_parallel_arg(-1)  # Error
#' validate_parallel_arg(NULL) # 0L (default sequential)
#' }
validate_parallel_arg <- function(parallel_value) {
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
