# scip-R

`scip-R` generates SCIP indexes for R workspaces. The output `.scip` file can
be consumed by code search and navigation tools that understand the Source Code
Intelligence Protocol.

The indexer detects R package metadata, discovers source files, parses R code,
resolves definitions and references, and serializes the result as protobuf using
the bundled `scip.proto` schema.

Since 0.9.57, nested function definitions and recursive or captured calls use
their nearest lexical binding. Package and base functions are shadowed locally;
explicit `pkg::name` calls still resolve to that package. Existing R indexes must
be refreshed with their original include scope to replace older merged identities.

## Installation

Install `Rapp` first, then install this package from the repository root:

```r
remotes::install_github("r-lib/Rapp")
remotes::install_local(".")
scipR::install_scip_cli()
```

Launcher installation is explicit: loading `scipR` leaves installed launchers
unchanged, including when loading a check or temporary library. Reinstall the
wrapper after moving the package to another library:

```r
scipR::install_scip_cli()
```

## Current-source example

From this checkout, generate an index with the current source modules:

```bash
Rscript example/generate_scip_index.R /tmp/scip-r-example.scip
```

The example indexes `R/**/*.R`. An optional second argument selects another
project while retaining that scope. Output paths are relative to the caller's
working directory. Sourced use accepts the same arguments:

```r
source("example/generate_scip_index.R")
generate_self_index("/tmp/example.scip", project_root = "/path/to/project")
```

## CLI Usage

Index the current project:

```bash
scip-r --root . --output index.scip
```

Index only selected source trees:

```bash
scip-r \
  --root /path/to/project \
  --output /path/to/project/.semquery/index.scip \
  --include "R/**/*.R" \
  --include "inst/**/*.R"
```

Use parallel symbol resolution:

```bash
scip-r --root /path/to/project --output index.scip --parallel 4
```

Show the installed package version:

```bash
scip-r --version
```

## Options

- `--root`: project directory to index. Defaults to the current directory.
- `--output`: destination `.scip` path. Defaults to `index.scip`.
- `--include`: glob pattern to include. Repeatable. When omitted, all
  non-ignored R files are indexed.
- `--namespace`: optional namespace prefix for emitted local symbols.
- `--project-name`: optional project label override for SCIP metadata.
- `--cache`: stores file hashes under `.scip-r/state.rds`.
- `--parallel`: number of `mirai` workers. Use `0` for sequential mode.
- `--version`: print the installed `scipR` version and exit.

File discovery honors `.gitignore`, `.Rbuildignore`, and default generated
directories. Use `--include` for large repositories with dependency folders such
as `renv/`; this avoids indexing vendored package sources.

## R API

The main exported entry point is `index_project()`:

```r
cfg <- list(
  root = normalizePath("."),
  output = normalizePath("index.scip", mustWork = FALSE),
  include = c("R/**/*.R", "inst/**/*.R"),
  cache = FALSE,
  parallel = 0L,
  namespace = NULL,
  `project-name` = NULL
)

scipR::index_project(cfg)
```

## Development

Run the test suite:

```bash
Rscript test/testthat.R
```

Build and check the package:

```bash
R CMD build .
R CMD check scipR_*.tar.gz --no-manual
```

Before indexing large projects, pass explicit `--include` patterns so dependency
directories such as `renv/` are not traversed.
