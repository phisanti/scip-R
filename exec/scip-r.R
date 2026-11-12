#!/usr/bin/env Rapp
#| name: scip-r
#| description: Generate SCIP index files for R projects
#| launcher:
#|   no-init-file: true

# =============================================================================
# scip-r CLI
# =============================================================================
#
# Generate SCIP (Source Code Intelligence Protocol) indexes for R projects.
# SCIP indexes enable code intelligence features like go-to-definition and
# find-references in tools like Sourcegraph.
#
# Usage examples:
#   scip-r --root=. --output=index.scip
#   scip-r --include "R/*.R" --include "tests/**/*.R" --output=index.scip
#   scip-r --root=/path/to/project --parallel=4 --output=index.scip
#
# Rapp's hash-pipe annotations (#|) provide automatic CLI parsing.
# For more information: https://github.com/r-lib/Rapp
#
# =============================================================================

# -----------------------------------------------------------------------------
# CLI Arguments
# -----------------------------------------------------------------------------

#| description: Project root directory to index
root <- "."

#| description: Output file path for the SCIP index
output <- "index.scip"

#| description: Glob patterns to include (repeatable). Only matching files are indexed.
include <- c()

#| description: Enable file discovery cache for incremental indexing
cache <- FALSE

#| description: Number of parallel workers (0 for sequential processing)
parallel <- 0L

# BUG FIX: --namespace was accepted and ignored, so the index came out
# byte-identical with or without it. The flag is removed; Rapp now rejects it.
# Anti-regression tests: tests/testthat/test-orchestration.R
#   - scip-r CLI rejects the removed --namespace flag
#   - index_project no longer forwards a namespace to the workspace

#| description: Project name in symbol IDs (defaults to the root directory name)
project_name <- ""

#| description: Print scip-r version and exit
version <- FALSE

# -----------------------------------------------------------------------------
# Argument Processing
# -----------------------------------------------------------------------------

if (isTRUE(version)) {
  desc <- utils::packageDescription("scipR", fields = "Version")
  cli::cli_text("scip-r {.val {desc}}")
  quit(save = "no", status = 0L)
}

# Normalize paths
root <- normalizePath(root, winslash = "/", mustWork = FALSE)
output <- normalizePath(output, winslash = "/", mustWork = FALSE)

# Validate parallel workers
parallel <- scipR::validate_args(parallel)

# Convert empty strings to NULL for optional parameters
project_name <- if (nzchar(project_name)) project_name else NULL

# Process include patterns (empty vector → NULL means "include all")
include <- include[nzchar(include)]
include <- if (length(include) > 0) include else NULL

# -----------------------------------------------------------------------------
# Execute Indexing Pipeline
# -----------------------------------------------------------------------------

cfg <- list(
  command = "create",
  root = root,
  output = output,
  include = include,
  cache = isTRUE(cache),
  parallel = parallel,
  `project-name` = project_name
)

scipR::index_project(cfg)
