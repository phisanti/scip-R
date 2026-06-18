#' @include parser.R
#' @include parse-tree-utils.R
NULL

# This module detects S3 object-oriented programming patterns in R code.
# It identifies S3 generics (functions using UseMethod) and S3 methods
# (functions following the generic.class naming convention).
# Enables accurate method dispatch navigation and class hierarchy tracking.

#' Detect if a function definition contains UseMethod call.
#'
#' Scans the function body for UseMethod() calls, indicating an S3 generic.
#'
#' @param func_id Numeric ID of the FUNCTION token in parse data.
#' @param data Parse data data.frame from getParseData().
#' @param lookup Lookup index from build_lookup_index() for O(1) id/parent lookups.
#' @param index Optional tree index from build_tree_index() for O(1) lookups.
#' @return List with is_generic (logical) and generic_name (character) if detected.
#' @keywords internal
detect_s3_generic <- function(func_id, data, lookup, index = NULL) {
  # The FUNCTION token's parent is an expr containing the entire function definition
  # The function body is a sibling expr node (with same parent)
  func_row <- rows_by_id_lookup(func_id, lookup)
  if (nrow(func_row) == 0) {
    return(list(is_generic = FALSE, generic_name = NULL))
  }

  func_parent <- func_row$parent[1]

  # Get all siblings of FUNCTION (nodes with same parent)
  siblings <- rows_by_parent_lookup(func_parent, lookup)

  # The function body is typically the last expr sibling after the closing ')'
  body_exprs <- siblings[siblings$token == "expr", , drop = FALSE]

  # Get descendants of all body expressions - use indexed version if available
  if (!is.null(index)) {
    # Use indexed traversal with list accumulation
    descendant_list <- list()
    for (body_id in body_exprs$id) {
      descendants <- get_descendants_indexed(body_id, index)
      if (nrow(descendants) > 0) {
        descendant_list[[length(descendant_list) + 1]] <- descendants
      }
    }
    all_descendants <- if (length(descendant_list) > 0) {
      do.call(rbind, descendant_list)
    } else {
      data[integer(0), , drop = FALSE]
    }
  } else {
    # Fallback to the order-preserving lookup traversal (legacy BFS order).
    descendant_list <- list()
    for (body_id in body_exprs$id) {
      descendants <- get_descendants_lookup(body_id, lookup)
      if (nrow(descendants) > 0) {
        descendant_list[[length(descendant_list) + 1]] <- descendants
      }
    }
    all_descendants <- if (length(descendant_list) > 0) {
      do.call(rbind, descendant_list)
    } else {
      data[integer(0), , drop = FALSE]
    }
  }

  # Look for SYMBOL_FUNCTION_CALL tokens that are "UseMethod"
  usemethod_rows <- all_descendants[all_descendants$token == "SYMBOL_FUNCTION_CALL" &
                                    all_descendants$text == "UseMethod", , drop = FALSE]

  if (nrow(usemethod_rows) == 0) {
    return(list(is_generic = FALSE, generic_name = NULL))
  }

  # Find the parent expr of UseMethod to get the argument
  usemethod_parent <- usemethod_rows$parent[1]
  parent_children <- rows_by_parent_lookup(usemethod_parent, lookup)

  # Look for STR_CONST (string literal) which is the generic name argument
  str_const_rows <- parent_children[parent_children$token == "STR_CONST", , drop = FALSE]

  if (nrow(str_const_rows) > 0) {
    # Extract string without quotes
    generic_name <- gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
  } else {
    # No explicit argument, generic name is the function name itself
    generic_name <- NULL
  }

  list(is_generic = TRUE, generic_name = generic_name)
}

#' Parse S3 method name to extract generic and class.
#'
#' Analyzes function names following the generic.class pattern.
#' Handles built-in generics and multi-part class names.
#'
#' @param func_name Character string of the function name.
#' @return List with is_method (logical), generic_name, and class_name.
#' @keywords internal
parse_s3_method_name <- function(func_name) {
  # List of common S3 generics (from base R and common packages)
  known_generics <- c(
    "print", "summary", "plot", "mean", "as.character", "as.data.frame",
    "as.list", "as.matrix", "coef", "coefficients", "fitted", "residuals",
    "predict", "update", "[", "[[", "$", "[<-", "[[<-", "$<-",
    "length", "names", "dim", "dimnames", "levels", "class",
    "str", "format", "head", "tail", "subset", "transform",
    "merge", "aggregate", "stack", "unstack", "reshape",
    "all.equal", "duplicated", "unique", "sort", "rank", "order",
    "rev", "rep", "seq", "diff", "na.omit", "na.exclude", "complete.cases"
  )
  
  # Check if the function name contains a dot
  if (!grepl("\\.", func_name)) {
    return(list(is_method = FALSE, generic_name = NULL, class_name = NULL))
  }
  
  # Try matching against known generics
  for (generic in known_generics) {
    pattern <- paste0("^", gsub("\\[", "\\\\[", gsub("\\$", "\\\\$", generic)), "\\.")
    if (grepl(pattern, func_name)) {
      class_name <- sub(pattern, "", func_name)
      return(list(
        is_method = TRUE,
        generic_name = generic,
        class_name = class_name
      ))
    }
  }
  
  # Fallback: try splitting on first dot
  # This is heuristic and may include false positives
  parts <- strsplit(func_name, "\\.", fixed = FALSE)[[1]]
  if (length(parts) >= 2) {
    # Only consider it a method if it looks like a function (not a variable)
    # We rely on caller context to determine if it's a function definition
    return(list(
      is_method = TRUE,
      generic_name = parts[1],
      class_name = paste(parts[-1], collapse = ".")
    ))
  }
  
  list(is_method = FALSE, generic_name = NULL, class_name = NULL)
}

#' Extract all S3 symbols from an AST module.
#'
#' Identifies S3 generics (functions using UseMethod) and S3 methods
#' (functions following generic.class naming convention).
#'
#' @param ast_module List produced by `parse_file()` containing parse data.
#' @return List of S3 symbol information with type, name, generic, and range.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' extract_s3_symbols(ast)
#' }
extract_s3_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # FAST PATH: Check if file has any UseMethod calls at all
  # If not, we can skip expensive generic detection for all functions
  usemethod_rows <- data[data$token == "SYMBOL_FUNCTION_CALL" & data$text == "UseMethod", , drop = FALSE]
  has_usemethod <- nrow(usemethod_rows) > 0

  # Build tree index + lookup only when this file actually has a generic to
  # detect (UseMethod present). The lookup feeds detect_s3_generic's O(1) scans;
  # building it for every file would add net overhead on S3-light files (the
  # main loop's single id scan below is cheap and not worth indexing).
  tree_index <- if (has_usemethod) build_tree_index(data) else NULL
  lookup <- if (has_usemethod) build_lookup_index(data) else NULL

  s3_symbols <- list()

  # Find all function definitions
  # Pattern: SYMBOL followed by assignment, then FUNCTION or OP-LAMBDA
  func_tokens <- c("FUNCTION", "OP-LAMBDA")

  # Find function token rows
  func_rows <- data[data$token %in% func_tokens, , drop = FALSE]

  for (idx in seq_len(nrow(func_rows))) {
    func_row <- func_rows[idx, ]
    func_id <- func_row$id

    # Find the assignment expression that contains this function
    # Navigate up the tree to find the assignment
    parent_id <- func_row$parent
    parent_expr <- data[data$id == parent_id, , drop = FALSE]

    if (nrow(parent_expr) == 0) next

    # Look for SYMBOL on the same line and before this function
    symbol_on_line <- data[data$line1 == func_row$line1 &
                           data$token == "SYMBOL" &
                           data$col1 < func_row$col1, , drop = FALSE]

    if (nrow(symbol_on_line) == 0) next

    # Get the closest SYMBOL (rightmost before FUNCTION)
    symbol_on_line <- symbol_on_line[order(symbol_on_line$col1, decreasing = TRUE), , drop = FALSE]
    func_name <- symbol_on_line$text[1]
    func_symbol_row <- symbol_on_line[1, ]

    # Check if this is an S3 generic (contains UseMethod)
    # Only do expensive check if file has UseMethod calls
    if (has_usemethod) {
      generic_info <- detect_s3_generic(func_id, data, lookup, tree_index)

      if (generic_info$is_generic) {
        # Determine the generic name
        # If UseMethod has an explicit argument, use that; otherwise use function name
        generic_name <- generic_info$generic_name %||% func_name

        s3_symbols[[length(s3_symbols) + 1]] <- list(
          type = "s3_generic",
          func_name = func_name,
          generic_name = generic_name,
          range = c(func_symbol_row$line1, func_symbol_row$col1,
                    func_symbol_row$line2, func_symbol_row$col2)
        )
      }
    }

    # Check if this is an S3 method (generic.class naming pattern)
    # This is fast - just string parsing, no tree traversal
    method_info <- parse_s3_method_name(func_name)

    if (method_info$is_method) {
      s3_symbols[[length(s3_symbols) + 1]] <- list(
        type = "s3_method",
        method_name = func_name,
        generic_name = method_info$generic_name,
        class_name = method_info$class_name,
        range = c(func_symbol_row$line1, func_symbol_row$col1,
                  func_symbol_row$line2, func_symbol_row$col2)
      )
    }
  }

  s3_symbols
}

#' Build SCIP symbol descriptor for an S3 symbol.
#'
#' Converts S3 generic or method into SCIP format with metadata.
#'
#' @param s3_info List from extract_s3_symbols with type, names, and range.
#' @param workspace Workspace context with package metadata.
#' @return List with SCIP symbol descriptor and metadata.
#' @keywords internal
s3_to_scip <- function(s3_info, workspace) {
  package_name <- workspace$metadata$namespace %||% "local"
  version <- workspace$metadata$version %||% "0.0.0"
  
  if (s3_info$type == "s3_generic") {
    # Format: scip-r <package> <version> <generic>().(s3_generic)
    descriptor <- sprintf("%s().(s3_generic)", s3_info$generic_name)
  } else if (s3_info$type == "s3_method") {
    # Format: scip-r <package> <version> <method>().(s3_method)
    descriptor <- sprintf("%s().(s3_method)", s3_info$method_name)
  } else {
    descriptor <- sprintf("%s().", s3_info$func_name %||% "unknown")
  }
  
  scip_symbol <- sprintf("scip-r %s %s %s", package_name, version, descriptor)
  
  list(
    symbol = scip_symbol,
    range = s3_info$range,
    metadata = list(
      s3_type = s3_info$type,
      generic_name = s3_info$generic_name,
      class_name = s3_info$class_name %||% NA
    )
  )
}

#' Convert S3 symbol info into SCIP-compatible symbol entries.
#'
#' Takes the output of extract_s3_symbols() and converts it into a list of
#' symbol entries suitable for SCIP serialization.
#'
#' @param ast_module Parsed AST module (from parse_file).
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List of symbol entries with symbol, display_name, kind fields.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_s3.R", relative_path = "my_s3.R"))
#' symbols <- build_s3_symbol_entries(ast, "my_s3.R", NULL)
#' }
build_s3_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  s3_infos <- extract_s3_symbols(ast_module)

  if (length(s3_infos) == 0) {
    return(list())
  }

  data <- ast_module$parse_data
  symbols <- list()
  
  for (s3_info in s3_infos) {
    # Build symbol ID
    if (s3_info$type == "s3_generic") {
      symbol_id <- build_s3_symbol_id(s3_info$func_name, relative_path, namespace)
      display_name <- s3_info$func_name
      kind <- "s3_generic"
    } else if (s3_info$type == "s3_method") {
      symbol_id <- build_s3_symbol_id(s3_info$method_name, relative_path, namespace)
      display_name <- s3_info$method_name
      kind <- "s3_method"
    } else {
      next  # Unknown S3 type, skip
    }
    
    symbols[[length(symbols) + 1]] <- list(
      symbol = symbol_id,
      display_name = display_name,
      kind = kind,
      documentation = extract_roxygen_documentation(data, s3_info$range[1])
    )
  }

  symbols
}

#' Build a SCIP symbol identifier for S3 symbols.
#'
#' Helper function to construct SCIP symbol IDs in the correct format.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_s3_symbol_id <- function(name, relative_path, namespace = NULL) {
  # Remove file extension and replace slashes/hyphens with underscores
  base_path <- gsub("\\.R$", "", relative_path)
  base_path <- gsub("/", "_", base_path)
  base_path <- gsub("-", "_", base_path)
  
  # Build local symbol ID (backtick-quote dotted R names per SCIP spec)
  esc_name <- escape_scip_identifier(name)
  if (!is.null(namespace) && nzchar(namespace)) {
    sprintf("%s %s#%s", namespace, base_path, esc_name)
  } else {
    sprintf("local %s#%s", base_path, esc_name)
  }
}
