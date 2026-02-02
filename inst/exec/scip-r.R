#!/usr/bin/env Rapp
#| name: scip-r
#| description: Generate SCIP index files for R projects
#| version: 0.9.1
#| launcher:
#|   no-init-file: true

# ============================================================================
# PRIMARY CLI ENTRYPOINT for scipR
# ============================================================================
# This is the main command-line interface for scip-r, powered by Rapp.
#
# Rapp's hash-pipe annotations (#|) automatically provide:
# - Command-line argument parsing with type validation
# - Help text generation (--help)
# - Version information (--version)
# - Boolean flags (--cache/--no-cache)
# - Default value handling
#
# This script constructs a configuration object and calls the orchestration
# layer directly, avoiding the double-parsing overhead of the deprecated
# run_cli() approach.
#
# For more information on Rapp syntax, see: https://github.com/r-lib/Rapp
# ============================================================================

#| description: Project root directory to index
root <- "."

#| description: Output file path for the SCIP index
output <- "index.scip"

#| description: Optional namespace prefix for local symbols
namespace <- ""

#| description: Override project name in metadata
project_name <- ""

#| description: Enable file discovery cache
cache <- FALSE

#| description: Number of parallel workers (0 for sequential)
parallel <- 0L

#| description: Glob patterns to include (repeatable). Only files matching at least one pattern are indexed.
include <- c()

# Normalize paths (Rapp provides raw string values)
root <- normalizePath(root, winslash = "/", mustWork = FALSE)
output <- normalizePath(output, winslash = "/", mustWork = FALSE)

# Validate and cap parallel workers at physical core count
parallel <- scipR:::validate_parallel_arg(parallel)

# Convert empty strings to NULL for optional parameters
namespace <- if (nzchar(namespace)) namespace else NULL
project_name <- if (nzchar(project_name)) project_name else NULL

# Process include patterns: filter out empty strings
include <- include[nzchar(include)]
if (length(include) == 0) include <- NULL

# Construct configuration object
cfg <- list(
  command = "create",
  root = root,
  output = output,
  namespace = namespace,
  `project-name` = project_name,
  cache = isTRUE(cache),
  parallel = parallel,
  include = include
)

# Execute indexing pipeline
scipR::index_project(cfg)
