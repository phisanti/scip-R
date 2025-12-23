#' @include workspace.R
#' @include files.R
#' @include parser.R
#' @include symbols.R
#' @include occurrences.R
#' @include diagnostics.R
#' @include serializer.R
NULL

# DEPRECATION NOTICE
# ==================
# This file contains the legacy CLI interface for scip-r.
# 
# STATUS: Being migrated to Rapp-based CLI in inst/exec/scip-r.R
#
# MIGRATION PLAN:
# - parse_cli_args()        -> DEPRECATED, remove after migration complete
# - run_cli()               -> DEPRECATED, remove after migration complete  
# - index_project()         -> KEEP, move to a new orchestration.R file
# - index_documents_sequential() -> KEEP, move to a new orchestration.R file
# - process_single_file()   -> KEEP, move to a new orchestration.R file
# - validate_parallel_arg() -> KEEP, move to a new orchestration.R file
# - install_scip_cli()      -> KEEP, this remains the primary installation function
#
# TODO after deprecation:
# 1. Remove parse_cli_args() and run_cli() from this file
# 2. Move index_project(), index_documents_sequential(), process_single_file(), 
#    and validate_parallel_arg() to R/orchestration.R
# 3. Remove exports for parse_cli_args() and run_cli() from NAMESPACE
# 4. Update DESCRIPTION Collate to include 'orchestration.R' instead of some cli.R functions
# 5. Keep install_scip_cli() in this file or move to R/install.R

# This script provides the command-line entry point for scip-r indexing.
# It parses user flags, coordinates the indexing pipeline, and reports status.
# Future implementations should expand orchestration logic for full coverage.

#' Parse command-line arguments for the scip-r CLI.
#'
#' @deprecated This function is deprecated and will be removed in a future version.
#' The new Rapp-based CLI in inst/exec/scip-r.R handles argument parsing automatically.
#' This function is kept temporarily for backward compatibility.
#'
#' The CLI exposes a single `create` subcommand today. Options:
#' - `--root` project directory to index (defaults to cwd)
#' - `--output` destination `.scip` file (defaults to `index.scip`)
#' - `--namespace` optional namespace prefix for emitted symbols
#' - `--project-name` override for workspace metadata name
#' - `--cache` enable cache hints for file discovery
#' - `--parallel` number of mirai daemons for symbol resolution (0 = sequential)
#'
#' @param args Character vector of command-line tokens, typically from `commandArgs(TRUE)`.
#' @return A named list describing the requested subcommand and options.
#' @export
#' @examples
#' parse_cli_args(c("create", "--root", ".", "--output", "index.scip"))
parse_cli_args <- function(args = commandArgs(TRUE)) {
  args <- args %||% character()
  args <- args[nzchar(args)]

  command <- "create"
  option_tokens <- args

  if (length(args) >= 1 && !startsWith(args[[1]], "-")) {
    command <- args[[1]]
    option_tokens <- args[-1]
  }

  if (!command %in% c("create", "index")) {
    stop(sprintf("Unknown subcommand '%s'. Expected 'create'.", command), call. = FALSE)
  }

  option_list <- list(
    optparse::make_option(
      c("-r", "--root"),
      type = "character",
      default = getwd(),
      help = "Project root to index [default: %default]"
    ),
    optparse::make_option(
      c("-o", "--output"),
      type = "character",
      default = "index.scip",
      help = "Destination .scip file [default: %default]"
    ),
    optparse::make_option(
      c("-n", "--namespace"),
      type = "character",
      default = NULL,
      help = "Optional namespace prefix for local symbols"
    ),
    optparse::make_option(
      "--project-name",
      type = "character",
      default = NULL,
      help = "Override project name reported in metadata"
    ),
    optparse::make_option(
      c("-c", "--cache"),
      action = "store_true",
      default = FALSE,
      help = "Enable cache hints when discovering files"
    ),
    optparse::make_option(
      c("-p", "--parallel"),
      type = "integer",
      default = 0L,
      help = "Number of mirai workers for resolution (0 = sequential)"
    )
  )

  parser <- optparse::OptionParser(
    usage = "scip-r create [options]",
    option_list = option_list,
    add_help_option = TRUE
  )

  opts <- optparse::parse_args(parser, args = option_tokens, positional_arguments = FALSE)

  opts$root <- normalizePath(opts$root, winslash = "/", mustWork = FALSE)
  opts$output <- normalizePath(opts$output, winslash = "/", mustWork = FALSE)
  opts$parallel <- validate_parallel_arg(opts$parallel)
  opts$cache <- isTRUE(opts$cache)
  opts$command <- command
  opts
}

#' Run the scip-r CLI index command.
#'
#' @deprecated This function is deprecated and will be removed in a future version.
#' Use the `scip-r` command-line tool directly instead (automatically installed with the package).
#' For programmatic access, call the underlying functions like `index_project()` directly.
#'
#' @param args Character vector of CLI tokens.
#' @return Invisibly returns the path to the generated `.scip` file on success.
#' @export
#' @examples
#' \dontrun{
#' # Deprecated - use `scip-r` command instead:
#' # scip-r --root . --output index.scip
#' 
#' # Legacy usage (still works but will be removed):
#' run_cli(c("create", "--root", ".", "--output", "index.scip"))
#' }
run_cli <- function(args = commandArgs(TRUE)) {
  cfg <- parse_cli_args(args)
  if (!cfg$command %in% c("create", "index")) {
    stop(sprintf("Unsupported command '%s'.", cfg$command), call. = FALSE)
  }

  index_project(cfg)
}

#' Index a project according to the parsed configuration.
#'
#' @note This function will be moved to a new R/orchestration.R file in a future version.
#' It orchestrates the indexing pipeline and will remain part of the public API.
#'
#' @param cfg Configuration list returned by `parse_cli_args()`.
#' @return Invisibly returns the output `.scip` path.
#' @keywords internal
index_project <- function(cfg) {
  workspace <- analyze_workspace(cfg$root, cfg$namespace)
  if (!is.null(cfg$`project-name`) && nzchar(cfg$`project-name`)) {
    workspace$metadata$project_name <- cfg$`project-name`
  }

  plan <- discover_files(workspace, cache_enabled = cfg$cache)
  effective_parallel <- cfg$parallel %||% 0L

  result <- if (effective_parallel > 0) {
    serialize_documents_mirai(plan$files, workspace, cfg$output, daemons = effective_parallel)
  } else {
    index_documents_sequential(plan$files, workspace, cfg$output)
  }

  invisible(result)
}

#' Sequential indexing fallback when parallel mode is disabled.
#'
#' @note This function will be moved to a new R/orchestration.R file in a future version.
#'
#' @param files File descriptors from `discover_files()`.
#' @param workspace Workspace list.
#' @param output_path Destination `.scip` path.
#' @return Output path (invisibly).
#' @keywords internal
index_documents_sequential <- function(files, workspace, output_path) {
  serializer <- init_serializer(output_path, workspace)
  on.exit(close_serializer(serializer), add = TRUE)

  all_external_refs <- list()

  for (file_info in files) {
    needs_index <- file_info$needs_index
    if (!is.null(needs_index) && isFALSE(needs_index)) {
      next
    }

    result <- tryCatch(process_single_file(file_info, workspace), error = function(e) {
      warning(
        sprintf(
          "File %s could not be processed: %s",
          file_info$relative_path %||% file_info$path,
          conditionMessage(e)
        ),
        call. = FALSE
      )
      NULL
    })

    if (is.null(result)) {
      next
    }

    write_document(serializer, result$document, diagnostics = result$diagnostics)
    if (length(result$external_refs) > 0) {
      all_external_refs <- merge_external_refs(all_external_refs, result$external_refs)
    }
  }

  write_externals(serializer, all_external_refs)
  result <- close_serializer(serializer)
  invisible(result)
}

#' Process a single file through parse -> resolve -> occurrences.
#'
#' @note This function will be moved to a new R/orchestration.R file in a future version.
#'
#' @param file_info File descriptor list.
#' @param workspace Workspace list.
#' @return List containing document/diagnostics/external_refs.
#' @keywords internal
process_single_file <- function(file_info, workspace) {
  ast <- parse_file(file_info)
  analysis <- resolve_symbols(ast, workspace, track_externals = TRUE)
  occurrences <- map_occurrences(analysis, ast)
  document <- build_document(file_info, occurrences, analysis)
  diagnostics <- collect_diagnostics(file_info, ast, analysis)
  list(document = document, diagnostics = diagnostics, external_refs = analysis$external_refs)
}

#' Validate and normalize the --parallel argument.
#'
#' @note This function will be moved to a new R/orchestration.R file in a future version.
#'
#' @param parallel_value Parsed integer from optparse.
#' @return Integer number of workers to request.
#' @keywords internal
validate_parallel_arg <- function(parallel_value) {
  if (is.null(parallel_value) || is.na(parallel_value)) {
    return(0L)
  }

  if (parallel_value < 0) {
    stop("--parallel must be >= 0", call. = FALSE)
  }

  if (parallel_value == 0) {
    return(0L)
  }

  cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores)) {
    return(as.integer(parallel_value))
  }

  as.integer(min(parallel_value, cores))
}

#' Install or update the scip-r CLI executable
#'
#' Normally, the `scip-r` command is installed automatically when the 
#' scipR package is installed. Use this function if you need to 
#' reinstall the CLI tool or install it to a custom location.
#'
#' @param destdir Destination directory for the CLI launcher. 
#'                If `NULL`, uses Rapp's default behavior (respects 
#'                `RAPP_INSTALL_DIR` environment variable and standard 
#'                XDG paths like `~/.local/bin`).
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' # Install to default location (usually ~/.local/bin on Unix)
#' install_scip_cli()
#' 
#' # Install to custom location
#' install_scip_cli(destdir = "~/my/custom/bin")
#' }
install_scip_cli <- function(destdir = NULL) {
  if (!requireNamespace("Rapp", quietly = TRUE)) {
    stop(
      "Rapp package required for CLI installation.\n",
      "Install with: remotes::install_github('r-lib/Rapp')",
      call. = FALSE
    )
  }
  
  if (is.null(destdir)) {
    Rapp::install_pkg_cli_apps("scipR")
    install_location <- Rapp::cli_app_dir()
  } else {
    Rapp::install_pkg_cli_apps("scipR", destdir = destdir)
    install_location <- destdir
  }
  
  message(sprintf("✓ scip-r CLI tool installed to: %s", install_location))
  message("  Make sure this directory is in your PATH.")
  
  invisible(TRUE)
}
