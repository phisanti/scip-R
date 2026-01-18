#' @include utils.R
#' @include parser.R
#' @include parse-tree-utils.R
NULL

# This module builds a package-wide symbol table for cross-file reference resolution.
# It collects all top-level definitions (functions, variables, classes) from all files
# in a package, enabling references in file A to resolve to definitions in file B.

# Well-known base R functions that should resolve to base:: package
# This is a curated list of commonly used base R functions
BASE_R_FUNCTIONS <- c(
  # Core functions
  "c", "list", "vector", "matrix", "array", "data.frame", "factor",
  "length", "nrow", "ncol", "dim", "names", "rownames", "colnames",
  "class", "typeof", "mode", "is.null", "is.na", "is.nan", "is.finite",
  "is.numeric", "is.character", "is.logical", "is.list", "is.vector",
  "is.matrix", "is.array", "is.data.frame", "is.factor", "is.function",
  "as.character", "as.numeric", "as.integer", "as.logical", "as.list",
  "as.vector", "as.matrix", "as.data.frame", "as.factor",
  # Math functions
  "sum", "mean", "median", "var", "sd", "min", "max", "range",
  "abs", "sqrt", "exp", "log", "log10", "log2", "sin", "cos", "tan",
  "floor", "ceiling", "round", "trunc", "sign",
  # String functions
  "paste", "paste0", "sprintf", "format", "nchar", "substr", "substring",
  "strsplit", "grep", "grepl", "sub", "gsub", "regexpr", "gregexpr",
  "tolower", "toupper", "trimws", "chartr",
  # Control flow
  "if", "else", "for", "while", "repeat", "break", "next", "return",
  "function", "switch", "ifelse",
  # Apply family
  "apply", "lapply", "sapply", "vapply", "mapply", "tapply", "by",
  "Map", "Reduce", "Filter", "Find", "Position",
  # Data manipulation
  "merge", "rbind", "cbind", "split", "unsplit", "subset", "transform",
  "with", "within", "attach", "detach",
  "head", "tail", "rev", "sort", "order", "rank", "unique", "duplicated",
  "which", "which.min", "which.max", "match", "pmatch", "%in%",
  # I/O functions
  "print", "cat", "message", "warning", "stop",
  "readLines", "writeLines", "read.csv", "write.csv", "read.table", "write.table",
  "readRDS", "saveRDS", "load", "save",
  "file", "open", "close", "readBin", "writeBin", "scan",
  "file.exists", "file.create", "file.remove", "file.copy", "file.rename",
  "file.path", "file.info", "dir.exists", "dir.create",
  "basename", "dirname", "normalizePath", "path.expand",
  # Environment functions
  "environment", "new.env", "parent.frame", "sys.call", "sys.function",
  "globalenv", "baseenv", "emptyenv",
  "get", "exists", "assign", "rm", "ls", "objects",
  "attach", "detach", "search", "searchpaths",
  # Attributes
  "attr", "attributes", "structure", "setattr",
  # Sequence generation
  "seq", "seq_len", "seq_along", "rep", "rep_len",
  # Logical operations
  "all", "any", "xor", "isTRUE", "isFALSE",
  # Set operations
  "union", "intersect", "setdiff", "setequal",
  # Type coercion
  "unlist", "stack", "unstack",
  # Debugging
  "browser", "debug", "undebug", "trace", "untrace", "traceback",
  # System
  "Sys.time", "Sys.Date", "Sys.getenv", "Sys.setenv", "Sys.info",
  "system", "system2", "shell",
  # Package management
  "library", "require", "loadNamespace", "attachNamespace",
  "installed.packages", "available.packages",
  # OOP
  "setClass", "setGeneric", "setMethod", "setRefClass",
  "R6Class",
  # Misc
  "options", "getOption", "setOption",
  "invisible", "nargs", "missing", "on.exit",
  "tryCatch", "try", "withCallingHandlers", "conditionMessage",
  "do.call", "Recall", "formals", "body", "args", "alist",
  "identical", "all.equal", "dput", "deparse", "parse", "eval", "evalq",
  "substitute", "quote", "bquote", "expression",
  "call", "match.call", "match.arg", "match.fun",
  "stopifnot", "inherits", "NextMethod", "UseMethod"
)

#' Build a package-wide symbol table from parsed files.
#'
#' Scans all files to collect top-level definitions (functions, variables, classes).
#' This table is used during reference resolution to map function calls to their
#' definitions, enabling cross-file reference tracking.
#'
#' @param parsed_files List of parsed AST modules from parse_file().
#' @param workspace Workspace metadata from analyze_workspace().
#' @return A list with:
#'   \describe{
#'     \item{definitions}{Named list mapping symbol name -> definition info}
#'     \item{exports}{Character vector of exported symbol names (from NAMESPACE)}
#'     \item{package_name}{Package name from workspace}
#'     \item{package_version}{Package version from workspace}
#'   }
#' @keywords internal
build_symbol_table <- function(parsed_files, workspace) {
  definitions <- list()
  # Use environment for O(1) lookups in Phase 3
  definitions_env <- new.env(hash = TRUE, parent = emptyenv())
  package_name <- workspace$metadata$project_name %||% "unknown"
  package_version <- workspace$metadata$project_version %||% "0.0.0-dev"

  # Parse NAMESPACE for exports (if available)
  t_ns <- bench_start("phase2_parse_namespace")
  exports <- parse_namespace_exports(workspace$metadata$project_root)
  bench_end(t_ns, list(export_count = length(exports)))

  # Create environment for fast export lookup
  exports_env <- new.env(hash = TRUE, parent = emptyenv())
  for (exp in exports) {
    exports_env[[exp]] <- TRUE
  }

  # Cache tree indices: file_path -> tree_index
  # This avoids rebuilding in Phase 3
  tree_indices <- list()

  # First pass: collect all top-level definitions from all files
  for (ast_module in parsed_files) {
    if (is.null(ast_module) || is.null(ast_module$parse_data)) {
      next
    }

    file_info <- ast_module$file_info %||% list(relative_path = "unknown")
    relative_path <- file_info$relative_path %||% basename(file_info$path %||% "unknown")

    # Find all top-level definitions in this file
    # extract_top_level_definitions builds tree_index internally, so we capture it
    t_extract <- bench_start("phase2_extract_definitions", list(file = relative_path))
    extract_result <- extract_top_level_definitions_with_index(ast_module)
    file_defs <- extract_result$definitions
    tree_indices[[relative_path]] <- extract_result$tree_index
    bench_end(t_extract, list(definition_count = length(file_defs)))

    t_agg <- bench_start("phase2_aggregate_definitions", list(file = relative_path))
    for (def in file_defs) {
      name <- def$name

      # If symbol already defined in another file, keep the first definition
      # (R loads files alphabetically in a package, but we'll use first-seen)
      # Use environment lookup for O(1) check
      if (is.null(definitions_env[[name]])) {
        def_record <- list(
          name = name,
          file = relative_path,
          kind = def$kind,
          line = def$line,
          exported = !is.null(exports_env[[name]]),
          symbol_id = build_global_symbol_id(
            name = name,
            package_name = package_name,
            package_version = package_version,
            kind = def$kind
          )
        )
        definitions[[name]] <- def_record
        definitions_env[[name]] <- def_record
      }
    }
    bench_end(t_agg, list(total_definitions = length(definitions)))
  }

  list(
    definitions = definitions,
    definitions_env = definitions_env,
    exports = exports,
    exports_env = exports_env,
    package_name = package_name,
    package_version = package_version,
    tree_indices = tree_indices
  )
}

#' Build a global SCIP symbol ID for a package-level definition.
#'
#' Creates symbol IDs that are consistent across files, following the format
#' used by scip-go and scip-python for cross-file reference resolution.
#'
#' @param name Symbol name.
#' @param package_name Package name.
#' @param package_version Package version string.
#' @param kind Symbol kind (function, variable, class, etc.).
#' @return SCIP symbol ID string.
#' @keywords internal
build_global_symbol_id <- function(name, package_name, package_version, kind = "function") {
  # Format: scip-r r <package> <version> <suffix>
  # Suffix: functions get "()", variables get ".", classes get "#"
  suffix <- switch(
    kind,
    `function` = paste0(name, "()."),
    call = paste0(name, "()."),
    s3_generic = paste0(name, "()."),
    s3_method = paste0(name, "()."),
    r6_class = paste0(name, "#"),
    s4_class = paste0(name, "#"),
    variable = paste0(name, "."),
    # Default to function-style for unknown kinds
    paste0(name, "().")
  )

  sprintf("scip-r r %s %s %s", package_name, package_version, suffix)
}

#' Build a symbol ID for an external package reference.
#'
#' @param name Symbol name.
#' @param package_name External package name.
#' @param package_version External package version (or "0.0.0" if unknown).
#' @param kind Symbol kind.
#' @return SCIP symbol ID string for external reference.
#' @keywords internal
build_external_symbol_id <- function(name, package_name, package_version = "0.0.0", kind = "function") {
  build_global_symbol_id(name, package_name, package_version, kind)
}

#' Build a local symbol ID for file-scoped symbols.
#'
#' Used for truly local symbols: function parameters, loop variables,
#' local variables inside functions. These should NOT be resolvable
#' from other files.
#'
#' @param name Symbol name.
#' @param relative_path File relative path.
#' @param scope_id Optional scope identifier (e.g., enclosing function name).
#' @return Local SCIP symbol ID string.
#' @keywords internal
build_local_symbol_id <- function(name, relative_path, scope_id = NULL) {
  sanitized_path <- gsub("[^A-Za-z0-9_/]", "_", relative_path)
  sanitized_name <- gsub("[^A-Za-z0-9_]", "_", name)

  if (!is.null(scope_id) && nzchar(scope_id)) {
    sanitized_scope <- gsub("[^A-Za-z0-9_]", "_", scope_id)
    local_id <- paste0(sanitized_path, "#", sanitized_scope, "#", sanitized_name)
  } else {
    local_id <- paste0(sanitized_path, "#", sanitized_name)
  }

  paste("local", local_id)
}

#' Extract top-level definitions from a parsed AST module with tree index.
#'
#' OPTIMIZATION: Returns both definitions AND the tree index so it can be
#' reused in Phase 3 (symbol resolution), avoiding redundant tree building.
#'
#' @param ast_module Parsed AST module from parse_file().
#' @return List with: definitions (list), tree_index (index structure).
#' @keywords internal
extract_top_level_definitions_with_index <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list(definitions = list(), tree_index = NULL))
  }

  definitions <- list()

  t_tree <- bench_start("phase2_build_tree_index", list(row_count = nrow(data)))
  tree_index <- build_tree_index(data)
  bench_end(t_tree)

  # Pre-compute set of node IDs inside function bodies
  # Use the pre-computed inside_function logical vector from tree_index for O(1) lookup
  t_precompute <- bench_start("phase2_precompute_function_scope", list(row_count = nrow(data)))
  # tree_index$inside_function is already a logical vector indexed by row
  # We just need to match assignment rows against it
  bench_end(t_precompute, list(inside_count = sum(tree_index$inside_function %||% 0)))

  # Find all assignment tokens
  t_find <- bench_start("phase2_find_assignments", list(row_count = nrow(data)))
  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN")
  assignment_rows <- data[data$token %in% assignment_tokens, , drop = FALSE]
  bench_end(t_find, list(assignment_count = nrow(assignment_rows)))

  # Pre-compute function tokens on each line for quick RHS check
  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_token_rows <- data[data$token %in% func_tokens, , drop = FALSE]

  t_analyze <- bench_start("phase2_analyze_assignments", list(assignment_count = nrow(assignment_rows)))
  # Get inside_function mask for O(1) lookup by row index
  inside_func_mask <- tree_index$inside_function %||% logical(nrow(data))

  for (i in seq_len(nrow(assignment_rows))) {
    assign_row <- assignment_rows[i, ]

    # O(1) check: is this assignment inside a function body?
    # Use row index from tree_index for direct lookup
    row_idx <- tree_index$rows[[as.character(assign_row$id)]]
    if (!is.na(row_idx) && isTRUE(inside_func_mask[row_idx])) {
      next
    }

    # Find the symbol being assigned to (should be on same line, before assignment)
    symbol_row <- find_assignment_target(assign_row, data, tree_index)
    if (is.null(symbol_row)) {
      next
    }

    name <- symbol_row$text

    # O(1) check: is this a function assignment?
    # Look for FUNCTION/OP-LAMBDA on same line, after the assignment operator
    kind <- if (is_function_assignment_fast(assign_row, func_token_rows)) {
      "function"
    } else {
      "variable"
    }

    definitions[[length(definitions) + 1]] <- list(
      name = name,
      kind = kind,
      line = symbol_row$line1
    )
  }
  bench_end(t_analyze, list(definition_count = length(definitions)))

  list(definitions = definitions, tree_index = tree_index)
}

#' Extract top-level definitions from a parsed AST module.
#'
#' Identifies functions, variables, and classes defined at the top level
#' (not nested inside other functions).
#'
#' @param ast_module Parsed AST module from parse_file().
#' @return List of definition records with name, kind, line fields.
#' @keywords internal
extract_top_level_definitions <- function(ast_module) {
  # Delegate to the new function but only return definitions for backward compat
  extract_top_level_definitions_with_index(ast_module)$definitions
}

#' Pre-compute all node IDs that are inside function bodies.
#'
#' OPTIMIZATION: Now uses the pre-computed inside_function vector from
#' build_tree_index() for O(n) extraction instead of O(n * nesting_depth)
#' descendant collection.
#'
#' @param parse_data Parse data data.frame.
#' @param tree_index Tree index from build_tree_index().
#' @return Character vector of node IDs inside function bodies.
#' @keywords internal
compute_inside_function_ids <- function(parse_data, tree_index) {
  # OPTIMIZATION: Use pre-computed inside_function vector if available
  # This is O(n) instead of O(n * nesting_depth)
  if (!is.null(tree_index$inside_function) && length(tree_index$inside_function) > 0) {
    inside_mask <- tree_index$inside_function
    return(as.character(parse_data$id[inside_mask]))
  }

  # Fallback to legacy implementation for backward compatibility
  compute_inside_function_ids_legacy(parse_data, tree_index)
}

#' Legacy implementation of inside function ID computation.
#'
#' Collects descendants for each function node. This has O(n * nesting_depth)
#' complexity for nested functions. Kept for backward compatibility when
#' tree_index doesn't have pre-computed inside_function.
#'
#' @param parse_data Parse data data.frame.
#' @param tree_index Tree index from build_tree_index().
#' @return Character vector of node IDs inside function bodies.
#' @keywords internal
compute_inside_function_ids_legacy <- function(parse_data, tree_index) {
  # Find all FUNCTION and OP-LAMBDA nodes
  func_tokens <- c("FUNCTION", "OP-LAMBDA")
  func_rows <- parse_data[parse_data$token %in% func_tokens, , drop = FALSE]

  if (nrow(func_rows) == 0) {
    return(character(0))
  }

  # Try C++ implementation if available
  if (!is.null(tree_index$cpp_index) && rcpp_available()) {
    result <- tryCatch(
      {
        ids <- compute_inside_function_ids_cpp(func_rows$id, tree_index$cpp_index)
        as.character(ids)
      },
      error = function(e) NULL
    )
    if (!is.null(result)) return(result)
  }

  # R fallback: collect all descendants of function nodes
  inside_ids <- character(0)
  for (func_id in func_rows$id) {
    desc_ids <- get_descendant_ids_indexed(func_id, tree_index)
    inside_ids <- c(inside_ids, as.character(desc_ids))
  }
  unique(inside_ids)
}

#' Fast check if assignment creates a function (O(1)).
#'
#' Checks if there's a FUNCTION or OP-LAMBDA token on the same line
#' as the assignment, positioned after the assignment operator.
#' This is a fast approximation that works for typical R code patterns.
#'
#' @param assign_row Parse data row for the assignment token.
#' @param func_token_rows Pre-filtered rows containing only FUNCTION/OP-LAMBDA tokens.
#' @return TRUE if this appears to be a function assignment.
#' @keywords internal
is_function_assignment_fast <- function(assign_row, func_token_rows) {
  # Look for FUNCTION/OP-LAMBDA on same line, after the assignment
  line <- assign_row$line1
  col <- assign_row$col1

  # Check if any function token is on same line and after assignment
  matches <- func_token_rows$line1 == line & func_token_rows$col1 > col
  any(matches)
}

#' Find the symbol being assigned to in an assignment expression.
#'
#' @param assign_row Parse data row for the assignment token.
#' @param parse_data Full parse data frame.
#' @param tree_index Tree index for lookups.
#' @return Parse data row for the target symbol, or NULL.
#' @keywords internal
find_assignment_target <- function(assign_row, parse_data, tree_index) {
  # Look for SYMBOL token on same line, before the assignment
  line <- assign_row$line1
  col <- assign_row$col1

  candidates <- parse_data[
    parse_data$line1 == line &
    parse_data$col1 < col &
    parse_data$token == "SYMBOL",
    , drop = FALSE
  ]

  if (nrow(candidates) == 0) {
    return(NULL)
  }

  # Return the closest symbol (highest column) before the assignment
  candidates[which.max(candidates$col1), ]
}

#' Parse NAMESPACE file to get exported symbols.
#'
#' @param project_root Project root path.
#' @return Character vector of exported symbol names.
#' @keywords internal
parse_namespace_exports <- function(project_root) {
  namespace_path <- file.path(project_root, "NAMESPACE")

  if (!file.exists(namespace_path)) {
    return(character())
  }

  lines <- readLines(namespace_path, warn = FALSE)
  exports <- character()

  for (line in lines) {
    # Match export() directives
    if (grepl("^export\\(", line)) {
      # Extract symbol names from export(name1, name2, ...)
      match <- regmatches(line, regexec("^export\\((.*)\\)", line))
      if (length(match[[1]]) > 1) {
        symbols <- strsplit(match[[1]][2], ",")[[1]]
        symbols <- trimws(symbols)
        # Remove quotes if present
        symbols <- gsub("^[\"']|[\"']$", "", symbols)
        exports <- c(exports, symbols)
      }
    }

    # Match exportPattern() - export all matching symbols
    # For now, we'll skip pattern matching and rely on explicit exports

    # Match S3method() exports
    if (grepl("^S3method\\(", line)) {
      match <- regmatches(line, regexec("^S3method\\(([^,]+),([^)]+)\\)", line))
      if (length(match[[1]]) > 2) {
        generic <- trimws(match[[1]][2])
        class_name <- trimws(match[[1]][3])
        method_name <- paste0(generic, ".", class_name)
        exports <- c(exports, method_name)
      }
    }
  }

  unique(exports)
}

#' Check if a function name is a known base R function.
#'
#' @param name Function name to check.
#' @return TRUE if it's a base R function, FALSE otherwise.
#' @keywords internal
is_base_r_function <- function(name) {
  name %in% BASE_R_FUNCTIONS
}

#' Get the R version string for base R symbols.
#'
#' Returns the major.minor version of R (e.g., "4.3") for use in
#' base R symbol IDs, similar to how scip-python uses Python version
#' for stdlib symbols.
#'
#' @return Character string with R version.
#' @keywords internal
get_r_version <- function() {

  paste(R.version$major, R.version$minor, sep = ".")
}

#' Resolve a symbol reference to its definition.
#'
#' Given a symbol name used in code, determines the appropriate symbol ID
#' by checking (in order):
#' 1. Package-qualified calls (pkg::func) - already handled elsewhere
#' 2. Package symbol table (cross-file resolution)
#' 3. Base R functions
#' 4. Falls back to local symbol if unresolved
#'
#' @param name Symbol name.
#' @param symbol_table Symbol table from build_symbol_table().
#' @param current_file Current file's relative path (for local fallback).
#' @param scope_id Optional scope identifier for local symbols.
#' @param is_definition Whether this is a definition occurrence.
#' @return List with symbol_id and resolved_to (package name or "local").
#' @keywords internal
resolve_symbol_reference <- function(name, symbol_table, current_file, scope_id = NULL, is_definition = FALSE) {
  # For definitions, always use the symbol table's ID if available
  # This ensures the definition and all references use the same ID
  # OPTIMIZATION: Use single lookup with NULL check instead of %in% + [[
  # This avoids two hash lookups (one for %in%, one for [[)
  def <- symbol_table$definitions[[name]]
  if (!is.null(def)) {
    return(list(
      symbol_id = def$symbol_id,
      resolved_to = symbol_table$package_name,
      is_exported = def$exported
    ))
  }

  # Check if it's a base R function
  if (is_base_r_function(name)) {
    return(list(
      symbol_id = build_external_symbol_id(name, "base", get_r_version(), "function"),
      resolved_to = "base",
      is_exported = TRUE
    ))
  }

  # Unresolved - use local symbol ID
  list(
    symbol_id = build_local_symbol_id(name, current_file, scope_id),
    resolved_to = "local",
    is_exported = FALSE
  )
}
