# Cross-file symbol table construction for scip-r.
# Collects package-level definitions, base-symbol allowlists, and reference
# resolution helpers used by the main resolver.
# Inventory: `BASE_R_FUNCTIONS`, `build_symbol_table()`,
# `resolve_symbol_reference()`.

#' @include utils.R
#' @include parser.R
#' @include parse-tree-utils.R
NULL

OO_CLASS_KINDS <- c("r6_class", "s4_class", "s7_class")

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

# Callable kinds whose same-named top-level definitions in different files
# get distinct symbols (see build_global_symbol_id()).
DISAMBIGUATED_KINDS <- c("function", "call", "s3_generic", "s3_method")

#' Key of one file's definition of a name in definitions_env.
#' @param name Symbol name.
#' @param relative_path Defining file.
#' @keywords internal
definition_key <- function(name, relative_path) paste0(relative_path, "\t", name)

#' Look up a package-level definition, preferring the referencing file's own.
#'
#' A name defined in several files has no plain-name entry, so a file that does
#' not define it gets NULL (unresolved) rather than another file's definition.
#' @param definitions_env Environment from build_symbol_table().
#' @param name Symbol name.
#' @param relative_path Referencing file.
#' @return Definition record or NULL.
#' @keywords internal
find_definition <- function(definitions_env, name, relative_path) {
  definitions_env[[definition_key(name, relative_path)]] %||% definitions_env[[name]]
}

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
#'     \item{definitions_env}{Environment for O(1) definition lookups}
#'     \item{exports_env}{Environment for O(1) export lookups}
#'     \item{package_name}{Package name from workspace}
#'     \item{package_version}{Package version from workspace}
#'     \item{tree_indices}{Per-file tree indices for phase 3 reuse}
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

  # First pass: collect all top-level definitions from all files.
  # The first definition of a name within one file wins.
  file_definitions <- list()
  seen_in_file <- new.env(hash = TRUE, parent = emptyenv())
  defining_files <- new.env(hash = TRUE, parent = emptyenv())
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

    for (def in file_defs) {
      key <- definition_key(def$name, relative_path)
      if (!is.null(seen_in_file[[key]])) next
      seen_in_file[[key]] <- TRUE
      file_definitions[[length(file_definitions) + 1L]] <- c(def, list(file = relative_path))
      defining_files[[def$name]] <- c(defining_files[[def$name]], relative_path)
    }
  }

  # Second pass: a name defined in one file keeps its plain ID and is found by
  # name from any file. A callable name defined in several files gets one
  # file-disambiguated ID per definition, found only through its file key.
  # BUG FIX (Architext-d6ihg): same-named top-level functions in different
  # files collapsed to the first-seen definition's symbol, so every caller in
  # every file was attributed to one file's function.
  # Root cause: the global symbol ID and definitions_env were keyed by name only.
  # Anti-regression tests: tests/testthat/test-symbol-table-duplicates.R
  #   - same top-level name in two files gets one symbol per file
  #   - a name defined in one file keeps its plain ID across files
  #   - a caller without its own definition of a duplicated name stays unresolved
  t_agg <- bench_start("phase2_aggregate_definitions")
  for (def in file_definitions) {
    name <- def$name
    ambiguous <- length(defining_files[[name]]) > 1L && def$kind %in% DISAMBIGUATED_KINDS
    key <- if (ambiguous) definition_key(name, def$file) else name
    # Variables and classes defined in several files keep first-seen identity.
    if (!is.null(definitions_env[[key]])) next
    def_record <- list(
      name = name,
      file = def$file,
      kind = def$kind,
      line = def$line,
      exported = !is.null(exports_env[[name]]),
      symbol_id = build_global_symbol_id(
        name = name,
        package_name = package_name,
        package_version = package_version,
        kind = def$kind,
        file = if (ambiguous) def$file
      )
    )
    definitions[[key]] <- def_record
    definitions_env[[key]] <- def_record
  }
  bench_end(t_agg, list(total_definitions = length(definitions)))

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

#' Resolve lexical bindings before package and base-R lookups.
#' @param data Parse-data frame.
#' @param index Cached tree index.
#' @param relative_path File identity for local symbols.
#' @return Character vector aligned with parse-data rows; empty means unbound.
#' @keywords internal
# BUG FIX: package lookup captured same-named local functions.
# Definitions and references must use the nearest binding, including through
# anonymous closures. Function-node keys separate sibling scopes; the full
# enclosing path separates repeated nested function names in their SCIP IDs.
# Anti-regression tests: tests/testthat/test-symbols-lexical-binding.R
#   - local function bindings shadow package and base names in both backends
#   - same named nested scopes keep distinct function bindings and captured calls
#   - lexical calls do not capture cross file package callers or unshadowed closures
build_lexical_symbol_map <- function(data, index, relative_path) {
  result <- rep("", nrow(data))
  function_rows <- which(data$token %in% c("FUNCTION", "OP-LAMBDA"))
  if (!length(function_rows)) return(result)
  parents <- new.env(hash = TRUE, parent = emptyenv())
  function_names <- new.env(hash = TRUE, parent = emptyenv())
  bindings <- new.env(hash = TRUE, parent = emptyenv())
  scopes <- new.env(hash = TRUE, parent = emptyenv())

  # The function expression's parent is outside its own scope, unlike its body.
  for (row in function_rows) {
    expr <- get_parent_id_indexed(data$id[row], index)
    parent_expr <- get_parent_id_indexed(expr, index)
    parents[[as.character(data$id[row])]] <- get_enclosing_function_indexed(parent_expr, index)
  }

  # Assignment identity comes from the AST target, not other tokens on its line.
  assignments <- list()
  for (row in which(data$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN"))) {
    if (data$text[row] %in% c("<<-", "->>")) next
    operands <- assignment_operands_indexed(row, data, index)
    if (is.null(operands)) next # Not a bare binding (e.g. object$field).
    literal <- function_literal_row_indexed(operands$value, index)
    is_function <- !is.na(literal)
    if (is_function) function_names[[as.character(data$id[literal])]] <- data$text[operands$target]
    assignments[[length(assignments) + 1L]] <- list(row = operands$target, function_binding = is_function)
  }

  scope_name <- function(scope) {
    key <- as.character(scope)
    cached <- scopes[[key]]
    if (!is.null(cached)) return(cached)
    name <- function_names[[key]]
    if (is.null(name)) {
      row <- get_row_indexed(scope, index)
      name <- paste0("@", data$line1[row], ":", data$col1[row])
    }
    parent <- parents[[key]]
    path <- if (is.na(parent)) name else paste0(scope_name(parent), "/", name)
    scopes[[key]] <- path
    path
  }
  register <- function(row, function_binding = FALSE) {
    scope <- index$enclosing_function[row]
    if (is.na(scope)) return(invisible(NULL))
    key <- as.character(scope)
    if (is.null(bindings[[key]])) bindings[[key]] <- new.env(hash = TRUE, parent = emptyenv())
    # Preserve existing variable/parameter IDs; function IDs need the full scope
    # to distinguish same-named nested functions in different enclosing closures.
    name <- if (function_binding) scope_name(scope) else function_names[[key]]
    bindings[[key]][[data$text[row]]] <- build_local_symbol_id(data$text[row], relative_path, name)
  }
  for (assignment in assignments) register(assignment$row, assignment$function_binding)
  for (row in which(data$token == "SYMBOL_FORMALS")) register(row)
  for (row in which(data$token == "SYMBOL")) {
    parent <- get_row_indexed(data$parent[row], index)
    if (!is.na(parent) && data$token[parent] == "forcond") register(row)
  }

  for (row in which(data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"))) {
    siblings <- get_children_indexed(data$parent[row], index)
    sibling_rows <- vapply(siblings, get_row_indexed, integer(1), index = index)
    if (any(data$token[sibling_rows] %in% c("NS_GET", "NS_GET_INT", "'$'", "'@'"))) next
    scope <- index$enclosing_function[row]
    while (!is.na(scope)) {
      key <- as.character(scope)
      local <- bindings[[key]]
      symbol <- if (!is.null(local)) local[[data$text[row]]] else NULL
      if (!is.null(symbol)) {
        result[row] <- symbol
        break
      }
      scope <- parents[[key]]
    }
  }
  result
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
#' @param file Relative path of the defining file, only for a callable name
#'   defined in several files. It becomes the method disambiguator, e.g.
#'   `.section(R:a.R).`; the Go decoder keeps it out of the display name.
#' @return SCIP symbol ID string.
#' @keywords internal
build_global_symbol_id <- function(name, package_name, package_version, kind = "function",
                                   file = NULL) {
  # Format: scip-r r <package> <version> <suffix>
  # Suffix: functions get "()", variables get ".", classes get "#"
  # Names with non-identifier chars (e.g. dots in R) must be backtick-quoted
  # per the SCIP spec (dots are descriptor delimiters, not identifier chars).
  esc_name <- escape_scip_identifier(name)
  # "/" would read as a descriptor namespace cut and a space would split the
  # symbol fields, so neither may appear in the disambiguator.
  call_suffix <- paste0(
    esc_name, "(",
    if (!is.null(file)) gsub("[^A-Za-z0-9_.:-]", "_", gsub("/", ":", file, fixed = TRUE)),
    ")."
  )
  if (kind %in% OO_CLASS_KINDS) {
    suffix <- paste0(esc_name, "#")
  } else {
    suffix <- switch(kind,
      `function` = call_suffix,
      call = call_suffix,
      s3_generic = call_suffix,
      s3_method = call_suffix,
      variable = paste0(esc_name, "."),
      # Default to function-style for unknown kinds
      paste0(esc_name, "().")
    )
  }

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
  # Use SCIP backtick-quoting to preserve dots (and other special chars)
  # instead of stripping them. The Go decoder handles backtick-quoted
  # identifiers in local IDs via parseDescriptors().
  esc_name <- escape_scip_identifier(name)

  if (!is.null(scope_id) && nzchar(scope_id)) {
    esc_scope <- escape_scip_identifier(scope_id)
    local_id <- paste0(sanitized_path, "#", esc_scope, "#", esc_name)
  } else {
    local_id <- paste0(sanitized_path, "#", esc_name)
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

  t_analyze <- bench_start("phase2_analyze_assignments", list(assignment_count = nrow(assignment_rows)))
  # Get inside_function mask for O(1) lookup by row index
  inside_func_mask <- tree_index$inside_function %||% logical(nrow(data))

  for (i in seq_len(nrow(assignment_rows))) {
    assign_row <- assignment_rows[i, ]

    # O(1) check: is this assignment inside a function body?
    # Use row index from tree_index for direct lookup
    row_idx <- get_row_indexed(assign_row$id, tree_index)
    if (!is.na(row_idx) && isTRUE(inside_func_mask[row_idx])) {
      next
    }

    # Find the symbol being assigned to (should be on same line, before assignment)
    symbol_row <- find_assignment_target(assign_row, data, tree_index)
    if (is.null(symbol_row)) {
      next
    }

    name <- symbol_row$text

    kind <- if (isTRUE(tree_index$function_bindings[get_row_indexed(symbol_row$id, tree_index)])) {
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
    if (!is.null(result)) {
      return(result)
    }
  }

  # R fallback: collect all descendants of function nodes
  inside_ids <- character(0)
  for (func_id in func_rows$id) {
    desc_ids <- get_descendant_ids_indexed(func_id, tree_index)
    inside_ids <- c(inside_ids, as.character(desc_ids))
  }
  unique(inside_ids)
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
      parse_data$token == "SYMBOL", ,
    drop = FALSE
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
