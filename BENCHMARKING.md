# Performance Benchmarking Guide

This document explains how to use the built-in benchmarking instrumentation in scip-R to identify performance bottlenecks.

## Quick Start

### Enable Benchmarking

Set the `SCIPR_BENCH` environment variable before running scip-R:

```r
# In R session
Sys.setenv(SCIPR_BENCH = "1")
scipR::run_cli()

# Or from command line
SCIPR_BENCH=1 Rscript -e "scipR::run_cli()" --args create --root /path/to/project --output index.scip
```

### Output Format

Benchmark data is printed to stdout in JSON Lines format (one JSON object per line):

```
[BENCH] {"op":"parse_file","file":"example.R","duration_ms":12.5,"timestamp":"2026-01-18T10:30:45.123"}
[BENCH] {"op":"build_tree_index","row_count":156,"duration_ms":2.3,"timestamp":"2026-01-18T10:30:45.125"}
[BENCH] {"op":"symbol_iteration","call_count":42,"timestamp":"2026-01-18T10:30:45.200"}
```

### Capturing and Analyzing Benchmarks

```bash
# Run with benchmarking and capture output
SCIPR_BENCH=1 Rscript -e "scipR::run_cli()" --args create --root /path/to/project --output index.scip 2>&1 | tee bench_output.log

# Extract just benchmark lines
grep '\[BENCH\]' bench_output.log > benchmarks.jsonl

# Analyze with jq (example: top 10 slowest operations)
grep '\[BENCH\]' bench_output.log | sed 's/\[BENCH\] //' | jq -s 'sort_by(.duration_ms) | reverse | .[0:10]'

# Sum total time per operation type
grep '\[BENCH\]' bench_output.log | sed 's/\[BENCH\] //' | jq -s 'group_by(.op) | map({op: .[0].op, total_ms: map(.duration_ms // 0) | add, count: length}) | sort_by(.total_ms) | reverse'

# Find high call counts (potential N+1 issues)
grep '\[BENCH\]' bench_output.log | sed 's/\[BENCH\] //' | jq -s 'map(select(.call_count != null)) | sort_by(.call_count) | reverse | .[0:10]'
```

## What Gets Benchmarked

### High-Level Pipeline Operations

- `index_project_total` - Complete indexing run
- `analyze_workspace` - Workspace analysis
- `discover_files` - File discovery
- `sequential_indexing` - Sequential mode total time
- `phase1_parsing` - Parsing phase (all files)
- `phase2_symbol_table` - Symbol table construction
- `phase3_resolve_serialize` - Resolution and serialization

### Parallel Mode Operations

- `mirai_phase1_parsing` - Parallel: parsing phase
- `mirai_phase2_symbol_table` - Parallel: symbol table
- `mirai_phase3_parallel_resolve` - Parallel: resolution phase
- `mirai_daemon_setup` - Worker setup time
- `mirai_task_dispatch` - Task dispatch time
- `mirai_task_collect` - Result collection time

### Per-File Operations

- `parse_file` - Individual file parsing (includes `file` metadata)
- `process_file_with_table` - File-level symbol resolution
- `write_document` - SCIP document serialization
- `write_externals` - External symbol writing

### Symbol Resolution Operations

- `build_tree_index` - Parse tree indexing (includes `row_count`)
- `build_relationships` - Symbol relationship building
- `extract_s3_symbols` - S3 method extraction
- `extract_r6_symbols` - R6 class extraction
- `extract_s4_symbols` - S4 class extraction
- `extract_shiny_symbols` - Shiny module extraction
- `extract_qualified_symbols` - Package-qualified symbol extraction

### Call Counters

These track iteration counts to detect potential performance issues:

- `build_tree_index_call` - How many times tree index is built
- `symbol_iteration` - Symbol processing loop iterations
- `has_descendant_check` - Tree descendant checks (function detection)
- `descendant_dfs_iteration` - DFS iterations during tree traversal

## Common Performance Issues to Look For

### 1. Excessive Tree Index Rebuilding

```bash
# Check if tree index is built more than once per file
grep '"op":"build_tree_index_call"' benchmarks.jsonl | jq -s 'length'
```

**Expected**: Should equal number of files being indexed.
**If higher**: Tree index is being rebuilt unnecessarily.

### 2. High DFS Iteration Counts

```bash
# Check descendant traversal iterations
grep '"op":"descendant_dfs_iteration"' benchmarks.jsonl | jq -s 'map(.call_count) | add'
```

**High counts indicate**: Complex ASTs or inefficient tree traversal patterns.
**Fix**: Consider caching or optimizing `has_descendant_of_type_indexed`.

### 3. Slow Symbol Extraction

```bash
# Find which symbol extraction is slowest
grep '"op":"extract_' benchmarks.jsonl | jq -s 'group_by(.op) | map({op: .[0].op, avg_ms: (map(.duration_ms) | add / length), total_ms: (map(.duration_ms) | add)}) | sort_by(.total_ms) | reverse'
```

**Common culprits**: R6 extraction (text matching overhead), S3 extraction (pattern complexity).

### 4. Parsing vs Resolution Time

```bash
# Compare parsing time to resolution time
grep '"op":"phase1_parsing"' benchmarks.jsonl | jq '.duration_ms'
grep '"op":"phase3_resolve_serialize"' benchmarks.jsonl | jq '.duration_ms'
```

**Expected**: Resolution should be slower than parsing.
**If parsing is slow**: Check file I/O, consider SSD.

### 5. Parallel Overhead

```bash
# Compare parallel phases to sequential baseline
grep '"op":"mirai_daemon_setup"' benchmarks.jsonl | jq '.duration_ms'
grep '"op":"mirai_task_dispatch"' benchmarks.jsonl | jq '.duration_ms'
grep '"op":"mirai_task_collect"' benchmarks.jsonl | jq '.duration_ms'
```

**High setup/dispatch/collect time**: Parallel overhead dominates. Use sequential mode for small repos.

## Example Analysis Workflow

1. **Run benchmark on target codebase**:
   ```bash
   SCIPR_BENCH=1 Rscript -e "scipR::run_cli()" --args create --root /path/to/large/project --output test.scip 2>&1 | tee bench.log
   ```

2. **Extract benchmark data**:
   ```bash
   grep '\[BENCH\]' bench.log | sed 's/\[BENCH\] //' > bench.jsonl
   ```

3. **Identify top 5 slowest operations**:
   ```bash
   jq -s 'sort_by(.duration_ms) | reverse | .[0:5] | .[] | "\(.op): \(.duration_ms)ms"' bench.jsonl
   ```

4. **Check for high iteration counts** (potential N+1):
   ```bash
   jq -s 'map(select(.call_count != null)) | sort_by(.call_count) | reverse | .[0:5]' bench.jsonl
   ```

5. **Compare per-file parse times** (identify slow files):
   ```bash
   jq -s 'map(select(.op == "parse_file")) | sort_by(.duration_ms) | reverse | .[0:10] | .[] | "\(.file): \(.duration_ms)ms"' bench.jsonl
   ```

6. **Phase breakdown** (where is time spent?):
   ```bash
   jq -s 'map(select(.op | startswith("phase"))) | .[] | "\(.op): \(.duration_ms)ms"' bench.jsonl
   ```

## Tips

- **Disable for production**: Benchmarking adds ~2-5% overhead. Only enable when profiling.
- **Large files**: JSON Lines format handles large outputs well. No need to load entire file in memory.
- **Repeated runs**: Run 3 times and average to account for variance.
- **Comparative benchmarking**: Run before/after optimization to measure impact.

## Integration with Testing

You can enable benchmarking in tests to track performance regressions:

```r
# In test file
test_that("indexing performance baseline", {
  Sys.setenv(SCIPR_BENCH = "1")

  output <- capture.output({
    scipR::index_project(list(
      root = test_project_path,
      output = tempfile(fileext = ".scip"),
      parallel = 0
    ))
  })

  bench_lines <- grep("\\[BENCH\\]", output, value = TRUE)
  # Parse and assert on timings...
})
```

## Disabling Benchmarking

```r
# Unset the environment variable
Sys.unsetenv("SCIPR_BENCH")

# Or explicitly set to 0
Sys.setenv(SCIPR_BENCH = "0")
```

When disabled, all `bench_*()` calls become no-ops with minimal overhead (<1% impact).
