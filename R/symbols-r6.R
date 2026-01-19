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

#' Extract member info from public/private/active lists.
#'
#' Parses 'public = list(...)', 'private = list(...)', or 'active = list(...)'
#' to extract member names and whether each is a method or field.
#'
#' @param descendants Parse data subset of R6Class call.
#' @param data Full parse data for context.
#' @param member_type Character: "public", "private", or "active".
#' @return List of member info (name, is_method) or NULL.
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

        # Extract member info from the list (name + is_method)
        members <- list()

        # Look for named list elements (name = value)
        eq_assign_rows <- list_descendants[list_descendants$token == "EQ_SUB", , drop = FALSE]

        for (j in seq_len(nrow(eq_assign_rows))) {
          eq_assign_row <- eq_assign_rows[j, ]
          eq_assign_parent <- eq_assign_row$parent

          # Get siblings of this assignment
          eq_assign_siblings <- data[data$parent == eq_assign_parent, , drop = FALSE]

          # Get left-hand side symbol (member name)
          lhs_mask <- eq_assign_siblings$token %in% c("SYMBOL", "SYMBOL_SUB")
          lhs_before <- eq_assign_siblings$line1 < eq_assign_row$line1 |
            (eq_assign_siblings$line1 == eq_assign_row$line1 &
               eq_assign_siblings$col1 < eq_assign_row$col1)
          symbol_lhs <- eq_assign_siblings[lhs_mask & lhs_before, , drop = FALSE]

          if (nrow(symbol_lhs) > 0) {
            symbol_lhs <- symbol_lhs[order(symbol_lhs$line1, symbol_lhs$col1), , drop = FALSE]
            member_name <- symbol_lhs$text[nrow(symbol_lhs)]

            # Check if RHS contains a FUNCTION token (method vs field)
            rhs_after <- eq_assign_siblings$line1 > eq_assign_row$line1 |
              (eq_assign_siblings$line1 == eq_assign_row$line1 &
                 eq_assign_siblings$col1 > eq_assign_row$col1)
            rhs_expr <- eq_assign_siblings[eq_assign_siblings$token == "expr" & rhs_after, , drop = FALSE]

            is_method <- FALSE
            if (nrow(rhs_expr) > 0) {
              rhs_expr <- rhs_expr[order(rhs_expr$line1, rhs_expr$col1), , drop = FALSE]
              rhs_descendants <- get_descendants(rhs_expr$id[1], data)
              is_method <- any(rhs_descendants$token == "FUNCTION")
            }

            members[[length(members) + 1]] <- list(
              name = member_name,
              is_method = is_method
            )
          }
        }

        if (length(members) > 0) {
          # Remove duplicates by name, keeping first occurrence
          seen <- character()
          unique_members <- list()
          for (m in members) {
            if (!(m$name %in% seen)) {
              seen <- c(seen, m$name)
              unique_members[[length(unique_members) + 1]] <- m
            }
          }
          return(unique_members)
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
    
    # Process public members (list of {name, is_method} info)
    if (!is.null(class_info$public_members) && length(class_info$public_members) > 0) {
      for (member_info in class_info$public_members) {
        member_name <- member_info$name
        is_method <- member_info$is_method

        # Determine kind and suffix based on is_method flag
        if (member_name == "initialize") {
          kind <- "r6_constructor"
          suffix <- "()."
        } else if (is_method) {
          kind <- "r6_method"
          suffix <- "()."
        } else {
          kind <- "r6_field"
          suffix <- "."
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

    # Process private members (list of {name, is_method} info)
    if (!is.null(class_info$private_members) && length(class_info$private_members) > 0) {
      for (member_info in class_info$private_members) {
        member_name <- member_info$name
        is_method <- member_info$is_method

        # Use is_method flag to determine kind
        if (is_method) {
          kind <- "r6_method"
          suffix <- "()."
        } else {
          kind <- "r6_field"
          suffix <- "."
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
    
    # Process active bindings (list of {name, is_method} info, treat as properties)
    if (!is.null(class_info$active_members) && length(class_info$active_members) > 0) {
      for (member_info in class_info$active_members) {
        member_name <- member_info$name
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
