# Produce a SCIP index using this checkout's current implementation.
# Direct use: Rscript example/generate_scip_index.R [output.scip] [project-root]
# Defaults index this checkout's R sources and write example/scip-r-example.scip.

#' Locate this script during direct execution or source().
#' @return Normalized absolute script path.
#' @keywords internal
discover_script_path <- function() {
  frames <- rev(sys.frames())
  for (frame in frames) {
    path <- get0("ofile", envir = frame, inherits = FALSE)
    if (is.character(path) && length(path) == 1L && nzchar(path)) {
      return(normalizePath(path, winslash = "/", mustWork = TRUE))
    }
  }
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (!length(file_arg)) {
    cli::cli_abort("Cannot locate the example; execute it with Rscript or source().")
  }
  normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE)
}

# Capture source()'s own path now: after it returns, --file names the caller.
.example_script_path <- discover_script_path()

#' Source current implementation modules into an isolated environment.
#' @param project_root This checkout's root containing R/.
#' @return Environment containing the current indexing entry point.
#' @keywords internal
source_project_modules <- function(project_root) {
  env <- new.env(parent = globalenv())
  scripts <- list.files(file.path(project_root, "R"), pattern = "\\.R$", full.names = TRUE)
  for (script in scripts) {
    sys.source(script, envir = env)
  }
  env
}

#' Generate a bounded index for this checkout or another R project.
#' @param output_path Output artifact, defaulting to example/scip-r-example.scip.
#' @param project_root Project to index, defaulting to this checkout.
#' @return Invisibly returns the normalized output path.
#' @examples
#' \dontrun{
#' generate_self_index()
#' }
# BUG FIX (Architext-6iwqq.1): the example called the removed run_cli API and
# ignored command arguments. A sourced example also located its caller script.
# Retain current-source execution through index_project(cfg), capture this
# file's identity at load time, and keep module bindings out of the caller.
# Anti-regression tests: test/testthat/test-generate-example.R
#   - sourced self-index example calls the current bounded configuration API
#   - sourced example indexes a synthetic project without changing caller bindings
#   - direct example execution honors output and project paths with spaces
generate_self_index <- function(output_path = NULL, project_root = NULL) {
  script_dir <- dirname(.example_script_path)
  checkout_root <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
  if (is.null(project_root)) project_root <- checkout_root
  project_root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  if (is.null(output_path)) {
    output_path <- file.path(script_dir, "scip-r-example.scip")
  }
  output_path <- path.expand(output_path)
  output_path <- file.path(
    normalizePath(dirname(output_path), winslash = "/", mustWork = TRUE),
    basename(output_path)
  )
  modules <- source_project_modules(checkout_root)
  modules$index_project(list(
    command = "create", root = project_root, output = output_path,
    include = "R/**/*.R", cache = FALSE, parallel = 0L,
    namespace = NULL, `project-name` = NULL
  ))
  invisible(output_path)
}

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) > 2L) {
    cli::cli_abort("Usage: Rscript generate_scip_index.R [output.scip] [project-root]")
  }
  generated <- generate_self_index(
    if (length(args) >= 1L) args[1] else NULL,
    if (length(args) >= 2L) args[2] else NULL
  )
  cli::cli_alert_success("Generated SCIP index at: {.path {generated}}")
}
