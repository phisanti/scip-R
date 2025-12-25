# scip-R

Experimental groundwork for a SCIP indexer targeting R projects.

## Installation

Install directly from GitHub using `remotes`:

```r
# Install Rapp first (required for CLI tool)
remotes::install_github('r-lib/Rapp')

# Install scipR (the scip-r CLI is automatically installed)
remotes::install_github('yourusername/scip-R')
```

**The `scip-r` command-line tool is automatically installed during package installation** via configure scripts. After installation completes, the `scip-r` command should be immediately available in your terminal (you may need to restart your terminal or start a new shell session).

## CLI Usage

### Command-Line Interface

Run `scip-r` directly from your terminal:

```bash
# Index a project with default settings
scip-r create --root /path/to/project --output index.scip

# Use parallel processing for faster indexing
scip-r create --root /path/to/project --parallel 4

# View all available options
scip-r create --help
```

### From R Console

You can also invoke the CLI from within R:

```r
# Load the package
library(scipR)

# Run indexing
scipR::run_cli(c("create", "--root", "/path/to/project", "--output", "index.scip"))
```

### Manual CLI Installation (if needed)

If the automatic installation didn't work or you need to reinstall the CLI tool:

```r
# Install to default location (~/.local/bin on Unix, %LOCALAPPDATA%\Programs\R\Rapp\bin on Windows)
scipR::install_scip_cli()

# Or install to a custom location
scipR::install_scip_cli(destdir = "~/my/custom/bin")
```

### Available Options

- `--root` project directory to index (defaults to current working directory)
- `--output` destination `.scip` path (absolute or relative)
- `--namespace` optional prefix for emitted symbols
- `--project-name` override for the project label stored in metadata
- `--cache` flag enabling cache hints during discovery (persists hashes under `.scip-r/state.rds`)
- `--parallel N` runs symbol resolution/serialization via `mirai` with up to `N` daemons.

Setting `--parallel 0` (the default) keeps the pipeline sequential, which is typically faster for very small repositories. Supplying a positive value enables the mirai backend described in `final_cli_tool_plan.md`, capped at the number of physical cores reported by `parallel::detectCores(logical = FALSE)`.

File discovery now honors `.gitignore`, `.Rbuildignore`, and a set of default folders (e.g., `.git`, `.Rproj.user`, `.renv`). When `--cache` is supplied, a manifest of file hashes is written to `.scip-r/state.rds` so subsequent runs can skip unchanged files by toggling their `needs_index` flag.

## Implementation Plan

### 1. Overview
Goal: ship `scip-r`, a CLI that produces `.scip` indexes for R workspaces so downstream tools (Zoekt + MCP) can answer definition/reference queries. The pipeline must detect R packages, parse source files, resolve bindings, and serialize SCIP messages reliably.

### 2. Core Modules

#### 2.1 CLI Orchestrator (`./R/cli.R`)
- **Why**: Provides the user entry point (`scip-r index`) and ensures reproducible runs across CI, editor, and MCP workflows.
- **What**: Parses flags (project root, output path, namespace, cache mode), determines workspace roots, coordinates per-module execution, handles errors.
- **How**: Implemented as an Rscript-friendly wrapper using `optparse`/`argparse`. Exposes structured logging via `logger` or base `message()`; orchestrates calls into downstream modules.

#### 2.2 Workspace & Environment Analyzer (`./R/workspace.R`)
- **Why**: SCIP metadata requires stable package identity and dependency resolution to map external symbols.
- **What**: Reads `DESCRIPTION`, `renv.lock`, `packrat.lock` if present, inspects `.Rproj` and `R/` directories, enumerates package dependencies with version pinning.
- **How**: Pure R implementation leveraging `desc`, `renv`, `pkgbuild`, and base `tools`. Provides:
  - `ProjectMetadata` (name, version, namespace, project root URI).
  - `DependencySet` for external symbol mapping.
  - `FileInclusionPolicy` respecting `.Rbuildignore`, `.gitignore`.

#### 2.3 File Discovery & Change Planner (`./R/files.R`)
- **Why**: Even initial implementation benefits from controlled file ordering and potential incremental runs.
- **What**: Walks directories, filters according to analyzer policy, optionally compares content hashes to skip unchanged files.
- **How**: Uses `fs::dir_ls` or `list.files` to enumerate sources, `digest` for hashing, caches manifest under `./.scip-r/state.rds`. Emits a queue of `SourceFile` descriptors (path, module membership, hash, needsIndex flag).

#### 2.4 Parser & AST Builder (`./R/parser.R`)
- **Why**: Symbol extraction needs structured syntax with source references.
- **What**: Wraps R’s internal parser with source reference retention (`keep.source=TRUE`) and optionally `codetools` for scoping utilities. Outputs a normalized IR (`AstNode`) capturing node type, text range, and binding context.
- **How**: Use R’s `parse` and `getParseData` to produce a table, enrich via custom visitors to respect R scoping rules (function, block, S3/S4 class constructs). Provide API:
  ```pseudo
  function parse_file(sourceFile) -> AstModule
  ```

#### 2.5 Binding & Symbol Resolver (`./R/symbols.R`)
- **Why**: R’s lexical scoping and dynamic dispatch require a pass that decides symbol kinds, descriptors, and container relationships.
- **What**: Walks `AstModule`, builds symbol table capturing packages, functions, classes (S3, S4, R6), variables, generics, and method implementations. Determines descriptors following SCIP grammar.
- **How**: Implement visitor that tracks environments:
  ```pseudo
  function resolve_symbols(ast, envStack):
      for node in ast.children:
          case node.type == "function_def":
              symbol <- makeSymbol(descriptor="Type/Method", suffix=Type)
              emitDefinition(node, symbol)
              pushEnv(envStack, symbol)
              resolve_symbols(node.body, envStack)
              popEnv(envStack)
          case node.type == "call" && isS3Method(node):
              registerImplementation(node, baseSymbol)
          ...
  ```
  Uses `codetools::walkCode` helpers or custom walker to detect S3 `UseMethod`, `setClass`, `setMethod`, R6 definitions.

#### 2.6 Occurrence Mapper & Range Generator (`./R/occurrences.R`)
- **Why**: SCIP `Occurrence` entries need precise UTF-32 (for R’s UTF handling) offsets aligned with source text.
- **What**: Converts symbol resolver events into definition/reference occurrences, attaches `enclosing_range` for call hierarchy, and tags syntax kinds.
- **How**: Track tokens via `getParseData`; compute byte/character positions; fallback to `stringi` for normalization. Provide:
  ```pseudo
  function add_occurrence(symbol, token, role):
      range <- computeRange(token, encoding="UTF32CodeUnitOffsetFromLineStart")
      occurrences.append(Occurrence(symbol, role, range, enclosingRange(token.parent)))
  ```

#### 2.7 External Symbol Catalog (`./R/external_symbols.R`)
- **Why**: Cross-project navigation needs to distinguish local vs external definitions.
- **What**: For each referenced package function/class not defined locally, record placeholder `SymbolInformation` in `Index.external_symbols`.
- **How**: Use `DependencySet` from analyzer, inspect `::` calls, `loadNamespace`, `require` statements. Provide mapping `external[package][symbol] -> metadata`.

#### 2.8 Diagnostic Aggregator (`./R/diagnostics.R`)
- **Why**: Surface parser/type errors as SCIP diagnostics improves agent feedback.
- **What**: Capture parse warnings, optionally integrate `lintr` checks on indexed files.
- **How**: Wrap parser/resolver in try/catch; translate R conditions to SCIP `Diagnostic` severity fields.

#### 2.9 SCIP Serializer & Output Writer (`./R/serializer.R`)
- **Why**: Final deliverable is a `.scip` protobuf stream.
- **What**: Streams `Index` message: metadata header, per-document payload, external symbols.
- **How**: Implemented in R using the generated protobuf bindings from `RProtoBuf` or `protobuf` package against `scip.proto`. Provides helpers to write metadata first, then documents, then external symbols. Pseudocode:
  ```pseudo
  function write_index(indexPath, metadata, documents, externals):
      writer <- newScipWriter(indexPath)
      writer.writeMetadata(metadata)
      for doc in documents:
          writer.writeDocument(doc)
      for sym in externals:
          writer.writeExternalSymbol(sym)
      writer.close()
  ```

### 3. Execution Flow (Pseudocode)
```pseudo
function main():
    cfg <- parseFlags()
    workspace <- analyzeWorkspace(cfg$root)
    files <- discoverFiles(workspace, cfg)
    writer <- openScipWriter(cfg$output, workspace$metadata)

    for file in files where file$needsIndex:
        ast <- parse_file(file)
        symbolGraph <- resolve_symbols(ast, newEnvStack(workspace))
        doc <- buildDocument(file, symbolGraph)
        writer.writeDocument(doc)

    externals <- collectExternalSymbols(symbolGraphSet, workspace$deps)
    writer.writeExternalSymbols(externals)
    writer.close()
```

### 4. Testing & Validation
- Unit tests per module: parser fixtures, symbol resolution for S3/S4/R6 patterns, descriptor generation.
- Golden `.scip` comparisons using `scip snapshot`.
- Integration tests indexing CRAN packages (e.g., `dplyr`, `shiny`) and verifying `scip print` contains expected definitions/references.

### 5. R to SCIP Symbol Mapping

The following table documents how R object types are mapped to SCIP protobuf symbol kinds and descriptors:

#### Basic R Constructs

| R Object Type | Kind Label | SCIP Kind | SCIP Descriptor | Example | Description |
|--------------|------------|-----------|-----------------|---------|-------------|
| Function | `function` | `Function` (17) | `name().` | `local path#myFunc().` | Regular R function definitions |
| Function Call | `call` | `Function` (17) | `name().` | - | Function call sites (references) |
| Variable | `variable` | `Variable` (61) | `name.` | `local path#myVar.` | Regular variables and loop variables |
| Parameter | `parameter` | `Parameter` (37) | `(name)` | `local path#(param)` | Function parameters (formals) |

#### S3 Object-Oriented Programming

| R Object Type | Kind Label | SCIP Kind | SCIP Descriptor | Example | Description |
|--------------|------------|-----------|-----------------|---------|-------------|
| S3 Generic | `s3_generic` | `Function` (17) | `name().` | `local path#print().` | Functions using `UseMethod()` |
| S3 Method | `s3_method` | `Method` (26) | `name().` | `local path#print.data.frame().` | S3 method implementations (generic.class) |

#### S4 Object-Oriented Programming

| R Object Type | Kind Label | SCIP Kind | SCIP Descriptor | Example | Description |
|--------------|------------|-----------|-----------------|---------|-------------|
| S4 Class | `s4_class` | `Class` (7) | `name#` | `local path#Person#` | Classes defined with `setClass()` |
| S4 Generic | `s4_generic` | `Function` (17) | `name().` | `local path#show().` | Generics defined with `setGeneric()` |
| S4 Method | `s4_method` | `Method` (26) | `name().` | `local path#show,Person().` | Methods defined with `setMethod()` |
| S4 Slot | `s4_slot` | `Field` (15) | `@name.` | `local path#Person#@age.` | Class slots/fields |

#### R6 Object-Oriented Programming

| R Object Type | Kind Label | SCIP Kind | SCIP Descriptor | Example | Description |
|--------------|------------|-----------|-----------------|---------|-------------|
| R6 Class | `r6_class` | `Class` (7) | `name#` | `local path#Counter#` | Classes defined with `R6Class()` |
| R6 Constructor | `r6_constructor` | `Constructor` (9) | `#name().` | `local path#Counter#initialize().` | R6 `initialize` method |
| R6 Method | `r6_method` | `Method` (26) | `#name().` | `local path#Counter#increment().` | R6 public/private methods |
| R6 Field | `r6_field` | `Field` (15) | `#name.` | `local path#Counter#.value.` | R6 private fields (typically prefixed with `.`) |
| R6 Active Binding | `r6_active` | `Property` (41) | `#name.` | `local path#Counter#count.` | R6 active bindings (computed properties) |

#### Shiny Application Framework

| R Object Type | Kind Label | SCIP Kind | SCIP Descriptor | Example | Description |
|--------------|------------|-----------|-----------------|---------|-------------|
| Shiny Module | `shiny_module` | `Module` (29) | `name#` | `local path#counterModule#` | Modules created with `moduleServer()` |
| Shiny Module Function | `shiny_module_function` | `Function` (17) | `name().` | `local path#counterModule().` | Generic module functions (suffixed with Module) |
| Shiny UI | `shiny_ui` | `Function` (17) | `name().` | `local path#counterUI().` | UI functions (suffixed with UI) |
| Shiny Server | `shiny_server` | `Function` (17) | `name().` | `local path#counterServer().` | Server functions (suffixed with Server) |
| Shiny Reactive | `shiny_reactive` | `Variable` (61) | `name.` | `local path#myReactive.` | Reactive values (`reactiveVal`, `reactiveValues`) |
| Shiny Observer | `shiny_observer` | `Event` (13) | `name.` | `local path#myObserver.` | Observers (`observe`, `observeEvent`) |
| Shiny Output | `shiny_output` | `Property` (41) | `name.` | `local path#plotOutput.` | Output render functions (`renderPlot`, etc.) |
| Shiny Namespace | `shiny_namespace` | `Namespace` (30) | `name/` | `local path#ns/` | Namespace functions (`NS()`) |

#### Symbol Descriptor Format

SCIP symbols follow the format: `<scheme> <package> <version> <descriptors>` or `local <local-id>` for local symbols.

For `scip-R`:
- **Local symbols** (defined within indexed files): `local <namespace> <file>#<name><suffix>`
  - Example: `local myPackage my_script#myFunction().`
- **External symbols** (from dependencies): `scip-r <package> <version> <name><suffix>`
  - Example: `scip-r dplyr 1.1.0 mutate().`

Where `<suffix>` is one of:
- `/` for namespaces
- `#` for types/classes
- `.` for terms (variables)
- `().` for methods/functions
- `(name)` for parameters
- `@name.` for slots (S4)
- `#name.` or `#name().` for class members (R6)

**Note**: The numeric values in parentheses (e.g., `Function (17)`) correspond to the SCIP protobuf enum values defined in `scip.proto` for `SymbolInformation.Kind`.

### 6. Future Enhancements (post-MVP)
- Incremental cache (content hash DB).
- Watch mode / daemon for rapid iteration.
- LSP façade for editors using the same symbol pipeline.
- Advanced dataflow for tidy-eval and dynamic symbol injection.
