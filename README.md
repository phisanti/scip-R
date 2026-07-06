# scip-R

`scip-R` generates SCIP indexes for R workspaces. The output `.scip` file can
be consumed by code search and navigation tools that understand the Source Code
Intelligence Protocol.

The indexer detects R package metadata, discovers source files, parses R code,
resolves definitions and references, and serializes the result as protobuf using
the bundled `scip.proto` schema.

## Installation

Install `Rapp` first, then install this package from the repository root:

```r
remotes::install_github("r-lib/Rapp")
remotes::install_local(".")
```

If the CLI wrapper is not on your `PATH` after installation, install it
explicitly:

```r
scipR::install_scip_cli()
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
