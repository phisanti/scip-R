#' @include parser.R
#' @include parse-tree-utils.R
#' @importFrom utils head
NULL

# This module detects R6 object-oriented programming patterns in R code.
# It identifies R6 class definitions created with R6Class() or R6::R6Class(),
# extracts their structure (public, private, active bindings), and tracks
# inheritance relationships. Based on languageserver's proven get_expr_type()
# pattern which uses regex: grepl("(R6:::?)?R6Class", func).

#' Extract R6 class definitions from an AST module.
#'
#' Scans parse data for R6Class() function calls and extracts class metadata
#' including class name, inheritance, and member visibility (public/private/active).
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @return List of R6 class information structures.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_r6.R", relative_path = "my_r6.R"))
#' r6_classes <- extract_r6_symbols(ast)
#' }
extract_r6_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  # FAST PATH: Check if file has any R6Class calls at all
  # Pre-filter on text to avoid expensive is_r6class_call checks for every function call
  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]

  if (nrow(func_calls) == 0) {
    return(list())
  }

  # Only consider calls that could be R6Class (text contains "R6Class")
  potential_r6_calls <- func_calls[grepl("R6Class", func_calls$text, fixed = TRUE), , drop = FALSE]

  if (nrow(potential_r6_calls) == 0) {
    return(list())
  }

  r6_classes <- list()

  for (i in seq_len(nrow(potential_r6_calls))) {
    call_row <- potential_r6_calls[i, ]

    # Check if this is an R6Class call (with or without namespace)
    if (!is_r6class_call(call_row, data)) {
      next
    }

    # Extract class information
    class_info <- extract_r6_class_info(call_row, data)

    if (!is.null(class_info)) {
      r6_classes[[length(r6_classes) + 1]] <- class_info
    }
  }

  r6_classes
}

#' Visualize the parse tree surrounding an R6Class() call.
#'
#' Diagnostic helper that prints the ancestor chain for the selected R6Class()
#' token and a grouped view of the expression subtree so we can inspect the
#' exact parse_data structure that extract_members_r6() needs to handle.
#'
#' @param ast_module Parsed module list returned by `parse_file()`.
#' @param which Integer index of the R6Class occurrence to inspect (defaults to 1).
#' @param output_file Optional path; when NULL a file named
#'   `r6_parse_tree_<which>.txt` is written to the current working directory.
#' @param max_rows_per_token Maximum rows to print per token group.
#' @return Invisibly returns a list containing the ancestor trace, subtree data,
#'   and the output file path for further inspection.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "check_r6.R", relative_path = "check_r6.R"))
#' visualize_r6_parse_tree(ast)
#' }
visualize_r6_parse_tree <- function(ast_module,
                                    which = 1,
                                    output_file = NULL,
                                    max_rows_per_token = 10) {
  data <- ast_module$parse_data

  if (is.null(data) || nrow(data) == 0) {
    cli::cli_abort("No parse data available; did {.fn parse_file} run with {.code keep.source = TRUE}?")
  }

  r6_rows <- data[data$token == "SYMBOL_FUNCTION_CALL" & data$text == "R6Class", , drop = FALSE]
  if (nrow(r6_rows) == 0) {
    cli::cli_abort("No {.fn R6Class} calls found in this module.")
  }

  if (which < 1 || which > nrow(r6_rows)) {
    cli::cli_abort("{.arg which} ({which}) must be between 1 and {nrow(r6_rows)}.")
  }

  call_row <- r6_rows[which, , drop = FALSE]

  # Build ancestor trace starting from the R6Class token.
  trace <- list()
  current_id <- call_row$id[1]
  level <- 1
  seen_ids <- integer()

  while (!is.na(current_id) && !(current_id %in% seen_ids)) {
    node <- data[data$id == current_id, , drop = FALSE]
    if (nrow(node) == 0) {
      break
    }

    node$trace_level <- level
    trace[[length(trace) + 1]] <- node
    seen_ids <- c(seen_ids, current_id)

    parent_id <- node$parent[1]
    if (is.na(parent_id) || parent_id == 0) {
      break
    }

    current_id <- parent_id
    level <- level + 1
  }

  trace_df <- do.call(rbind, trace)

  # Select the outermost expr ancestor to anchor the visualization.
  expr_nodes <- trace_df[trace_df$token == "expr", , drop = FALSE]
  if (nrow(expr_nodes) > 0) {
    expr_nodes <- expr_nodes[order(expr_nodes$trace_level, decreasing = TRUE), , drop = FALSE]
    call_expr <- expr_nodes[1, , drop = FALSE]
  } else {
    call_expr <- trace_df[nrow(trace_df), , drop = FALSE]
  }

  call_expr_id <- call_expr$id[1]
  descendants <- get_descendants(call_expr_id, data)
  call_expr_data <- call_expr[, colnames(data), drop = FALSE]

  if (nrow(descendants) > 0) {
    subtree <- rbind(call_expr_data, descendants)
    subtree <- subtree[!duplicated(subtree$id), , drop = FALSE]
  } else {
    subtree <- call_expr_data
  }

  # Compute depth relative to the selected expression.
  depth_map <- new.env(parent = emptyenv())
  depth_map[[as.character(call_expr_id)]] <- 0L

  compute_depth <- function(node_id) {
    key <- as.character(node_id)
    cached <- depth_map[[key]]
    if (!is.null(cached)) {
      return(cached)
    }

    row <- subtree[subtree$id == node_id, , drop = FALSE]
    if (nrow(row) == 0) {
      depth_map[[key]] <- 0L
      return(0L)
    }

    parent_id <- row$parent[1]
    if (is.na(parent_id) || !(parent_id %in% subtree$id)) {
      depth_map[[key]] <- 0L
      return(0L)
    }

    parent_depth <- compute_depth(parent_id)
    depth_map[[key]] <- parent_depth + 1L
    parent_depth + 1L
  }

  subtree$depth <- vapply(subtree$id, compute_depth, integer(1))

  # Decide where to write the report.
  if (is.null(output_file)) {
    output_file <- file.path(getwd(), sprintf("r6_parse_tree_%d.txt", which))
  }

  con <- file(output_file, open = "wt")
  on.exit(close(con), add = TRUE)

  write_lines <- function(lines) {
    writeLines(lines, con = con)
  }

  module_path <- ast_module$file_info$path %||% "<unknown file>"
  write_lines(sprintf("R6Class Parse Tree Visualization (%s)\n", module_path))
  write_lines(sprintf("Selected R6Class occurrence: %d of %d\n", which, nrow(r6_rows)))
  write_lines("Ancestor trace (starting at R6Class token):")

  trace_df <- trace_df[order(trace_df$trace_level), , drop = FALSE]
  for (i in seq_len(nrow(trace_df))) {
    row <- trace_df[i, ]
    line <- sprintf(
      "  L%-2d id=%-5s token=%-20s text='%s' parent=%s line=%s:%s",
      row$trace_level,
      row$id,
      row$token,
      ifelse(is.na(row$text), "", row$text),
      ifelse(is.na(row$parent), "NA", row$parent),
      ifelse(is.na(row$line1), "?", row$line1),
      ifelse(is.na(row$col1), "?", row$col1)
    )
    write_lines(line)
  }

  write_lines("\nExpression subtree grouped by token:")

  subtree <- subtree[order(subtree$token, subtree$line1, subtree$col1), , drop = FALSE]
  token_groups <- split(subtree, subtree$token)
  token_names <- names(token_groups)

  for (token in token_names) {
    group <- token_groups[[token]]
    total_rows <- nrow(group)
    write_lines(sprintf("\n[%s] (%d rows)", token, total_rows))

    sample_rows <- head(group, max_rows_per_token)
    for (i in seq_len(nrow(sample_rows))) {
      row <- sample_rows[i, ]
      indent <- paste0(strrep(" ", min(row$depth, 10) * 2))
      line <- sprintf(
        "%s- id=%-5s depth=%-2s line=%s:%s text='%s' parent=%s",
        indent,
        row$id,
        row$depth,
        ifelse(is.na(row$line1), "?", row$line1),
        ifelse(is.na(row$col1), "?", row$col1),
        ifelse(is.na(row$text), "", row$text),
        ifelse(is.na(row$parent), "NA", row$parent)
      )
      write_lines(line)
    }

    if (total_rows > max_rows_per_token) {
      write_lines(sprintf("  ... truncated %d rows ...", total_rows - max_rows_per_token))
    }
  }

  write_lines("\nTip: Increase `max_rows_per_token` to dump full groups.")
  cli::cli_alert_success("Parse tree visualization written to {.path {output_file}}")

  invisible(list(
    trace = trace_df,
    subtree = subtree,
    output = output_file
  ))
}

#' Check if a function call is an R6Class invocation.
#'
#' Detects both R6::R6Class and R6Class patterns (after library(R6)).
#' Based on languageserver's pattern: grepl("(R6:::?)?R6Class", func)
#'
#' @param call_row Parse data row for the SYMBOL_FUNCTION_CALL token.
#' @param data Full parse data data.frame.
#' @return Logical indicating if this is an R6Class call.
#' @keywords internal
is_r6class_call <- function(call_row, data) {
  # Direct R6Class call (after library(R6))
  if (call_row$text == "R6Class") {
    return(TRUE)
  }

  # Check for R6::R6Class or R6:::R6Class pattern
  # The namespace operator creates a structure like:
  # expr
  #   SYMBOL_PACKAGE (R6)
  #   NS_GET (:: or :::)
  #   SYMBOL_FUNCTION_CALL (R6Class)

  # Get parent (should be expr containing the namespace call)
  parent_id <- call_row$parent
  parent_children <- data[data$parent == parent_id, , drop = FALSE]

  # Look for namespace operators
  has_ns_operator <- any(parent_children$token %in% c("NS_GET", "NS_GET_INT"))

  if (has_ns_operator && call_row$text == "R6Class") {
    # Verify the package name is R6 (token is SYMBOL_PACKAGE)
    pkg_rows <- parent_children[parent_children$token == "SYMBOL_PACKAGE", , drop = FALSE]
    if (nrow(pkg_rows) > 0 && any(pkg_rows$text == "R6")) {
      return(TRUE)
    }
  }

  FALSE
}

#' Extract detailed information from an R6Class call.
#'
#' Parses the arguments to R6Class() to extract class name, inheritance,
#' and member definitions (public, private, active).
#'
#' @param call_row Parse data row for the R6Class function call.
#' @param data Full parse data data.frame.
#' @return List with class metadata or NULL if parsing fails.
#' @keywords internal
extract_r6_class_info <- function(call_row, data) {
  # The R6Class token's parent is the namespace expr (R6::R6Class)
  # We need the grandparent which contains the full function call with arguments
  namespace_expr_id <- call_row$parent
  namespace_expr <- data[data$id == namespace_expr_id, , drop = FALSE]

  if (nrow(namespace_expr) == 0) {
    return(NULL)
  }

  # Get the grandparent (the full call expression)
  full_call_id <- namespace_expr$parent[1]
  full_call_expr <- data[data$id == full_call_id, , drop = FALSE]

  if (nrow(full_call_expr) == 0) {
    return(NULL)
  }

  # Get all descendants of the full call to find arguments
  descendants <- get_descendants(full_call_id, data)

  # Extract class name (first STR_CONST argument)
  class_name <- extract_class_name_r6(descendants)

  if (is.null(class_name) || !nzchar(class_name)) {
    return(NULL)
  }

  # Extract inheritance information
  inherits_from <- extract_inherit_r6(descendants, data)

  # Extract public members
  public_members <- extract_members_r6(descendants, data, "public")

  # Extract private members
  private_members <- extract_members_r6(descendants, data, "private")

  # Extract active bindings
  active_members <- extract_members_r6(descendants, data, "active")

  list(
    type = "r6_class",
    class_name = class_name,
    inherits_from = inherits_from,
    public_members = public_members,
    private_members = private_members,
    active_members = active_members,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

# Note: get_descendants() is now imported from parse-tree-utils.R
# (formerly get_descendants_r6, now unified across all modules)

#' Extract class name from R6Class call arguments.
#'
#' The class name is the first string literal argument to R6Class().
#'
#' @param descendants Parse data subset of R6Class call descendants.
#' @return Character string of class name or NULL.
#' @keywords internal
extract_class_name_r6 <- function(descendants) {
  # Find first STR_CONST token (the class name)
  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]

  if (nrow(str_const_rows) == 0) {
    return(NULL)
  }

  # Remove quotes from string literal
  class_name <- gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
  class_name
}

#' Extract inheritance information from R6Class call.
#'
#' Looks for 'inherit = ParentClass' argument pattern.
#'
#' @param descendants Parse data subset of R6Class call.
#' @param data Full parse data for context.
#' @return Character string of parent class name or NULL.
#' @keywords internal
extract_inherit_r6 <- function(descendants, data) {
  # Look for EQ_SUB token (= in named argument)
  eq_sub_rows <- descendants[descendants$token == "EQ_SUB", , drop = FALSE]

  if (nrow(eq_sub_rows) == 0) {
    return(NULL)
  }

  # Find 'inherit' argument
  for (i in seq_len(nrow(eq_sub_rows))) {
    eq_row <- eq_sub_rows[i, ]
    eq_parent <- eq_row$parent

    # Get siblings of the = operator
    siblings <- data[data$parent == eq_parent, , drop = FALSE]

    # Check if left side is 'inherit' (token is SYMBOL_SUB for named arguments)
    # Need to find the SYMBOL_SUB that's IMMEDIATELY before this EQ_SUB (same line, just before)
    symbol_left <- siblings[siblings$token == "SYMBOL_SUB" &
                            siblings$line1 == eq_row$line1 &
                            siblings$col1 < eq_row$col1, , drop = FALSE]

    if (nrow(symbol_left) > 0 && any(symbol_left$text == "inherit")) {
      # Get the right side expr (on the same line or just after the =)
      expr_right <- siblings[siblings$token == "expr" &
                             siblings$line1 == eq_row$line1 &
                             siblings$col1 > eq_row$col1, , drop = FALSE]

      if (nrow(expr_right) > 0) {
        # The parent class name is a SYMBOL inside this expr
        expr_children <- data[data$parent == expr_right$id[1], , drop = FALSE]
        symbol_rows <- expr_children[expr_children$token == "SYMBOL", , drop = FALSE]

        if (nrow(symbol_rows) > 0) {
          return(symbol_rows$text[1])
        }
      }
    }
  }

  NULL
}

#' Extract member names from public/private/active lists.
#'
#' Parses 'public = list(...)', 'private = list(...)', or 'active = list(...)'
#' to extract member names.
#'
#' @param descendants Parse data subset of R6Class call.
#' @param data Full parse data for context.
#' @param member_type Character: "public", "private", or "active".
#' @return Character vector of member names or NULL.
#' @keywords internal
extract_members_r6 <- function(descendants, data, member_type) {
  # Look for EQ_SUB token followed by member_type SYMBOL
  eq_sub_rows <- descendants[descendants$token == "EQ_SUB", , drop = FALSE]

  if (nrow(eq_sub_rows) == 0) {
    return(NULL)
  }

  # Find the member_type = list(...) pattern
  for (i in seq_len(nrow(eq_sub_rows))) {
    eq_row <- eq_sub_rows[i, ]
    eq_parent <- eq_row$parent

    # Get siblings
    siblings <- data[data$parent == eq_parent, , drop = FALSE]

    # Check if left side matches member_type
    token_mask <- siblings$token %in% c("SYMBOL", "SYMBOL_SUB")
    before_eq <- siblings$line1 < eq_row$line1 |
      (siblings$line1 == eq_row$line1 & siblings$col1 < eq_row$col1)
    symbol_left <- siblings[token_mask & before_eq, , drop = FALSE]

    if (nrow(symbol_left) > 0) {
      symbol_left <- symbol_left[order(symbol_left$line1, symbol_left$col1), , drop = FALSE]
      last_symbol <- symbol_left[nrow(symbol_left), , drop = FALSE]

      if (last_symbol$text != member_type) {
        next
      }

      # Found the member_type argument, now extract list members
      # Get descendants of the right-hand side expr
      after_eq <- siblings$line1 > eq_row$line1 |
        (siblings$line1 == eq_row$line1 & siblings$col1 > eq_row$col1)
      expr_right <- siblings[siblings$token == "expr" & after_eq, , drop = FALSE]

      if (nrow(expr_right) > 0) {
        expr_right <- expr_right[order(expr_right$line1, expr_right$col1), , drop = FALSE]
        list_descendants <- get_descendants(expr_right$id[1], data)

        # Extract SYMBOL names from the list (member names)
        # These appear before = or as standalone elements
        member_names <- character()

        # Look for named list elements (name = value)
        eq_assign_rows <- list_descendants[list_descendants$token == "EQ_SUB", , drop = FALSE]

        for (j in seq_len(nrow(eq_assign_rows))) {
          eq_assign_row <- eq_assign_rows[j, ]
          eq_assign_parent <- eq_assign_row$parent

          # Get left-hand side symbol
          eq_assign_siblings <- data[data$parent == eq_assign_parent, , drop = FALSE]
          lhs_mask <- eq_assign_siblings$token %in% c("SYMBOL", "SYMBOL_SUB")
          lhs_before <- eq_assign_siblings$line1 < eq_assign_row$line1 |
            (eq_assign_siblings$line1 == eq_assign_row$line1 &
               eq_assign_siblings$col1 < eq_assign_row$col1)
          symbol_lhs <- eq_assign_siblings[lhs_mask & lhs_before, , drop = FALSE]

          if (nrow(symbol_lhs) > 0) {
            symbol_lhs <- symbol_lhs[order(symbol_lhs$line1, symbol_lhs$col1), , drop = FALSE]
            member_names <- c(member_names, symbol_lhs$text[nrow(symbol_lhs)])
          }
        }

        if (length(member_names) > 0) {
          return(unique(member_names))
        }
      }
    }
  }

  NULL
}

#' Convert R6 class info into SCIP-compatible symbol entries.
#'
#' Takes the output of extract_r6_symbols() and converts it into a list of
#' symbol entries suitable for SCIP serialization, including the class itself,
#' its methods, and its fields.
#'
#' @param ast_module Parsed AST module (from parse_file).
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List of symbol entries with symbol, display_name, kind fields.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_r6.R", relative_path = "my_r6.R"))
#' symbols <- build_r6_symbol_entries(ast, "my_r6.R", NULL)
#' }
build_r6_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  r6_classes <- extract_r6_symbols(ast_module)
  
  if (length(r6_classes) == 0) {
    return(list())
  }
  
  symbols <- list()
  
  for (class_info in r6_classes) {
    # Build class symbol ID using the helper from symbols.R
    class_symbol <- build_r6_symbol_id(class_info$class_name, relative_path, namespace)
    
    # Add class symbol
    symbols[[length(symbols) + 1]] <- list(
      symbol = class_symbol,
      display_name = class_info$class_name,
      kind = "r6_class",
      documentation = NULL
    )
    
    # Process public members
    if (!is.null(class_info$public_members) && length(class_info$public_members) > 0) {
      for (member_name in class_info$public_members) {
        # Determine if it's a method or field based on naming
        if (member_name == "initialize") {
          kind <- "r6_constructor"
          suffix <- "()."
        } else {
          # Default to method (most common case)
          kind <- "r6_method"
          suffix <- "()."
        }
        
        member_symbol <- paste0(class_symbol, "#", member_name, suffix)
        
        symbols[[length(symbols) + 1]] <- list(
          symbol = member_symbol,
          display_name = member_name,
          kind = kind,
          documentation = NULL,
          enclosing_symbol = class_symbol
        )
      }
    }
    
    # Process private members
    if (!is.null(class_info$private_members) && length(class_info$private_members) > 0) {
      for (member_name in class_info$private_members) {
        # Private members that start with . are typically fields
        if (startsWith(member_name, ".")) {
          kind <- "r6_field"
          suffix <- "."
        } else {
          kind <- "r6_method"
          suffix <- "()."
        }
        
        member_symbol <- paste0(class_symbol, "#", member_name, suffix)
        
        symbols[[length(symbols) + 1]] <- list(
          symbol = member_symbol,
          display_name = member_name,
          kind = kind,
          documentation = NULL,
          enclosing_symbol = class_symbol
        )
      }
    }
    
    # Process active bindings (treat as properties)
    if (!is.null(class_info$active_members) && length(class_info$active_members) > 0) {
      for (member_name in class_info$active_members) {
        member_symbol <- paste0(class_symbol, "#", member_name, ".")
        
        symbols[[length(symbols) + 1]] <- list(
          symbol = member_symbol,
          display_name = member_name,
          kind = "r6_active",
          documentation = NULL,
          enclosing_symbol = class_symbol
        )
      }
    }
  }
  
  symbols
}

#' Build a SCIP symbol identifier for R6 classes.
#'
#' Helper function to construct SCIP symbol IDs in the correct format.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_r6_symbol_id <- function(name, relative_path, namespace = NULL) {
  # Remove file extension and replace slashes/hyphens with underscores
  base_path <- gsub("\\.R$", "", relative_path)
  base_path <- gsub("/", "_", base_path)
  base_path <- gsub("-", "_", base_path)
  
  # Build local symbol ID
  if (!is.null(namespace) && nzchar(namespace)) {
    sprintf("%s %s#%s", namespace, base_path, name)
  } else {
    sprintf("local %s#%s", base_path, name)
  }
}
