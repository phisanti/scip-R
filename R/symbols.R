#' @include utils.R
#' @include parser.R
#' @include parse-tree-utils.R
#' @include symbols-qualified.R
#' @include symbols-s3.R
#' @include symbols-r6.R
NULL

# This script derives symbol definitions and relationships from parsed modules.
# It maps R constructs into SCIP-compatible descriptors and metadata bundles.
# Integrated with symbols-qualified.R for package-qualified symbol tracking.

#' Resolve symbols present in an AST module.
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @param workspace Workspace context with metadata and dependencies.
#' @param track_externals Logical; if TRUE, track package-qualified external symbols (default: TRUE).
#' @return A list containing symbol information, occurrence records, and external references.
#' @export
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "script.R", relative_path = "script.R"))
#' resolve_symbols(ast, analyze_workspace(tempdir()))
#' resolve_symbols(ast, analyze_workspace(tempdir()), track_externals = FALSE)
#' }
resolve_symbols <- function(ast_module, workspace, track_externals = TRUE) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list(symbols = list(), occurrences = list(), external_refs = list()))
  }

  file_info <- ast_module$file_info %||% list(relative_path = basename(ast_module$file_info$path))
  relative_path <- file_info$relative_path %||% basename(file_info$path)

  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  assignment_rows <- data[data$token %in% assignment_tokens, , drop = FALSE]
  symbols <- list()
  occurrences <- list()
  seen_symbols <- character()

  # FIX 1: Include SYMBOL_FORMALS to detect function parameters
  symbol_rows <- data[data$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL", "SYMBOL_FORMALS"), ]
  symbol_rows <- symbol_rows[order(symbol_rows$line1, symbol_rows$col1), , drop = FALSE]

  for (idx in seq_len(nrow(symbol_rows))) {
    row <- symbol_rows[idx, ]
    name <- row$text
    symbol_id <- build_symbol_id(name, relative_path, workspace$metadata$namespace)

    # Determine if this is a definition
    # SYMBOL_FORMALS are always definitions (function parameters)
    if (row$token == "SYMBOL_FORMALS") {
      is_definition <- !(symbol_id %in% seen_symbols)
    } else {
      # FIX 2: Check for loop variables (forcond parent)
      parent_row <- get_parent(row$id, data)
      is_loop_var <- nrow(parent_row) > 0 && parent_row$token[1] == "forcond"

      # Check for assignment
      on_assignment_line <- any(assignment_rows$line1 == row$line1 & assignment_rows$col1 > row$col1)

      is_definition <- (is_loop_var || (row$token == "SYMBOL" && on_assignment_line)) &&
                       !(symbol_id %in% seen_symbols)
    }

    role <- if (is_definition) "definition" else "reference"
    kind <- determine_symbol_kind(row, data)

    if (is_definition) {
      symbols[[length(symbols) + 1]] <- list(
        symbol = symbol_id,
        display_name = name,
        kind = kind,
        documentation = NULL
      )
      seen_symbols <- c(seen_symbols, symbol_id)
    }

    occurrences[[length(occurrences) + 1]] <- list(
      symbol = symbol_id,
      range = c(row$line1, row$col1, row$line2, row$col2),
      role = role
    )
  }

  # Extract S3 symbols (generics and methods)
  s3_symbols <- build_s3_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
  if (length(s3_symbols) > 0) {
    # Merge S3 symbols into main symbol list
    # Check for duplicates - prefer S3-specific kind over generic function
    for (s3_sym in s3_symbols) {
      existing_idx <- which(sapply(symbols, function(s) s$symbol == s3_sym$symbol))
      
      if (length(existing_idx) > 0) {
        # Update existing symbol with S3-specific kind
        symbols[[existing_idx]]$kind <- s3_sym$kind
      } else {
        # Add new S3 symbol
        symbols[[length(symbols) + 1]] <- s3_sym
      }
    }
  }

  # Extract R6 class symbols (classes, methods, fields)
  r6_symbols <- build_r6_symbol_entries(ast_module, relative_path, workspace$metadata$namespace)
  
  if (length(r6_symbols) > 0) {
    # Merge R6 symbols into main symbol list
    for (r6_sym in r6_symbols) {
      symbols[[length(symbols) + 1]] <- r6_sym
    }
  }

  # Extract package-qualified external symbols (pkg::symbol and pkg:::symbol)
  external_refs <- list()
  if (track_externals) {
    qualified_symbols <- extract_qualified_symbols(ast_module)
    
    # Build external references grouped by package
    for (qual in qualified_symbols) {
      pkg_name <- qual$package
      
      # Find or create package entry in external_refs
      if (length(external_refs) == 0) {
        pkg_idx <- integer(0)
      } else {
        pkg_matches <- sapply(external_refs, function(x) identical(x$package, pkg_name))
        pkg_idx <- which(pkg_matches)
      }
      
      if (length(pkg_idx) == 0) {
        # Create new package entry
        external_refs[[length(external_refs) + 1]] <- list(
          package = pkg_name,
          version = workspace$dependencies[[pkg_name]] %||% "0.0.0",
          symbols = list(list(
            name = qual$token,
            accessor = qual$accessor,
            is_internal = (qual$accessor == ":::"),
            range = qual$range
          ))
        )
      } else {
        # Add to existing package entry
        external_refs[[pkg_idx[1]]]$symbols[[length(external_refs[[pkg_idx[1]]]$symbols) + 1]] <- list(
          name = qual$token,
          accessor = qual$accessor,
          is_internal = (qual$accessor == ":::"),
          range = qual$range
        )
      }
    }
  }

  list(
    symbols = symbols,
    occurrences = occurrences,
    external_refs = external_refs
  )
}

#' Build a unique SCIP symbol identifier.
#'
#' @param name Symbol name.
#' @param relative_path Document-relative path.
#' @param namespace Optional namespace prefix.
#' @return Symbol identifier string following SCIP local symbol format.
#' @keywords internal
build_symbol_id <- function(name, relative_path, namespace = NULL) {
  ns_prefix <- if (!is.null(namespace) && nzchar(namespace)) paste0(namespace, "/") else ""
  sanitized_path <- gsub("[^A-Za-z0-9_/]", "_", relative_path)
  sanitized_name <- gsub("[^A-Za-z0-9_]", "_", name)
  local_id <- paste0(ns_prefix, sanitized_path, "#", sanitized_name)
  paste("local", local_id)
}

#' Check if symbol is defined as a function.
#'
#' Uses languageserver pattern: find assignment on same line, check RHS for FUNCTION token.
#' This is more robust than checking siblings since it properly traverses the parse tree.
#'
#' @param row Parse-data row for the symbol.
#' @param parse_data Full parse-data frame.
#' @return Logical indicating if this is a function definition.
#' @keywords internal
is_function_definition <- function(row, parse_data) {
  # Find assignment on same line after this symbol
  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")
  assignment_rows <- find_on_same_line(
    row$id,
    parse_data,
    assignment_tokens,
    "after"
  )

  if (nrow(assignment_rows) == 0) {
    return(FALSE)
  }

  # Get the closest assignment (smallest column position)
  assignment_rows <- assignment_rows[order(assignment_rows$col1), , drop = FALSE]
  assignment_row <- assignment_rows[1, ]

  # Get the expression containing the assignment
  assignment_expr <- get_assignment_expr(assignment_row$id, parse_data)

  if (nrow(assignment_expr) == 0) {
    return(FALSE)
  }

  # Check if the expression contains FUNCTION or OP-LAMBDA
  has_descendant_of_type(
    assignment_expr$id,
    c("FUNCTION", "OP-LAMBDA"),
    parse_data
  )
}

#' Determine the general kind for a symbol token.
#'
#' @param row Parse-data row for the symbol.
#' @param parse_data Full parse-data frame.
#' @return Character label describing the symbol kind.
#' @keywords internal
determine_symbol_kind <- function(row, parse_data) {
  # FIX 1: Function parameters (SYMBOL_FORMALS) - HIGH IMPACT
  if (row$token == "SYMBOL_FORMALS") {
    return("parameter")
  }

  # FIX 2: Loop variables (forcond parent) - HIGH IMPACT
  parent_row <- get_parent(row$id, parse_data)
  if (nrow(parent_row) > 0 && parent_row$token[1] == "forcond") {
    return("variable")  # Loop variables are a special kind of variable definition
  }

  # FIX 3: Robust function detection - MEDIUM IMPACT
  if (is_function_definition(row, parse_data)) {
    return("function")
  }

  # Function calls
  if (row$token == "SYMBOL_FUNCTION_CALL") {
    return("call")
  }

  # Default: regular variable
  "variable"
}

#' Merge external reference lists from multiple files.
#'
#' Combines external reference lists, merging symbols for the same package.
#'
#' @param refs1 First external references list.
#' @param refs2 Second external references list to merge.
#' @return Merged external references list.
#' @export
#' @keywords internal
merge_external_refs <- function(refs1, refs2) {
  if (length(refs1) == 0) return(refs2)
  if (length(refs2) == 0) return(refs1)
  
  result <- refs1
  
  for (pkg2 in refs2) {
    # Find matching package in refs1
    pkg_idx <- which(sapply(result, function(x) x$package == pkg2$package))
    
    if (length(pkg_idx) == 0) {
      # New package, add it
      result[[length(result) + 1]] <- pkg2
    } else {
      # Merge symbols for existing package
      result[[pkg_idx]]$symbols <- c(result[[pkg_idx]]$symbols, pkg2$symbols)
    }
  }
  
  result
}
