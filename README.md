# scip-R

`scip-R` generates [SCIP](https://github.com/scip-code/scip) indexes for R
projects. The `.scip` file records every definition and reference with a stable
symbol ID, so code search and navigation tools can answer "where is this
defined" and "who calls this" without parsing R themselves.

Resolution covers what R code actually uses:

- functions, variables and parameters, across files and through closures
- S3 generics and methods, S4 classes, generics and methods, S7 and R6 classes
- Shiny modules, reactives, observers and outputs
- plumber route handlers
- `pkg::name` and `pkg:::name` calls to other packages

## Installation

```r
remotes::install_github("phisanti/scip-R")
scipR::install_scip_cli()
```

`install_scip_cli()` puts the `scip-r` command in Rapp's bin directory
(`~/.local/bin` on macOS and Linux); make sure it is on your `PATH`. Run it
again after upgrading R or moving the package to another library. Loading the
package never creates or changes a launcher.

scipR writes protobuf through RProtoBuf, which needs the Protocol Buffers
library. On Debian or Ubuntu install `libprotobuf-dev` and
`protobuf-compiler` first; on macOS, `brew install protobuf`.

## Usage

Index a package:

```bash
scip-r --root path/to/project --output index.scip --include "R/**/*.R"
```

Repeat `--include` for more source trees, and add workers for large projects:

```bash
scip-r \
  --root path/to/project \
  --output index.scip \
  --include "R/**/*.R" \
  --include "inst/**/*.R" \
  --parallel 4
```

Always pass `--include` for real projects. Without it every R file not excluded
by `.gitignore` or `.Rbuildignore` is indexed, including the dependency sources
renv keeps under `renv/`.

scip-r exits with a non-zero status when `--root` does not exist or no file
matches the patterns, so it is safe to use in Makefiles and CI jobs.

| Option | Meaning |
|---|---|
| `--root` | Project directory to index. Defaults to the current directory. |
| `--output` | Destination `.scip` file. Defaults to `index.scip`. |
| `--include` | Glob relative to the root; repeatable. `**` matches any depth. |
| `--parallel` | Number of `mirai` workers; `0` (default) is sequential. Falls back to sequential if workers cannot start. |
| `--cache` | Store file hashes under `.scip-r/` between runs. |
| `--project-name` | Project name in global symbol IDs and metadata. Defaults to the name of the root directory. |
| `--version` | Print the installed version and exit. |

The vignette walks through setup, common project layouts, and reading an index
back from R:

```r
vignette("scipR", package = "scipR")
```

Vignettes are built only when asked:
`remotes::install_github("phisanti/scip-R", build_vignettes = TRUE)`.

## R API

`index_project()` takes the same settings as the command line:

```r
scipR::index_project(list(
  root = normalizePath("."),
  output = normalizePath("index.scip", mustWork = FALSE),
  include = c("R/**/*.R", "inst/**/*.R"),
  cache = FALSE,
  parallel = 0L,
  `project-name` = NULL
))
```

## Development

```r
testthat::test_local()   # compiles the C++ sources and runs the suite
```

```bash
R CMD build .
R CMD check scipR_*.tar.gz --no-manual
```

## License

MIT
