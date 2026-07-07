# R6 symbol extraction for object-oriented R code.
# Detects classes, inheritance, and members from R6 definitions for SCIP.
# Inventory: `extract_r6_symbols()`, `build_r6_symbol_entries()`,
# `build_r6_symbol_id()`.

#' @include parser.R
#' @include parse-tree-utils.R
#' @include parse-lookup.R
#' @importFrom utils head
NULL

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

  # Build a lightweight id/parent lookup once per file to replace the repeated
  # linear `data[data$id == X, ]` / `data[data$parent == X, ]` scans below with
  # O(1) lookups (row order preserved -> identical output).
  lookup <- build_lookup_index(data)

  for (i in seq_len(nrow(potential_r6_calls))) {
    call_row <- potential_r6_calls[i, ]

    # Check if this is an R6Class call (with or without namespace)
    if (!is_r6class_call(call_row, data, lookup)) {
      next
    }

    # Extract class information
    class_info <- extract_r6_class_info(call_row, data, lookup)

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
#' @param lookup Lookup index from build_lookup_index() for O(1) child lookups.
#' @return Logical indicating if this is an R6Class call.
#' @keywords internal
is_r6class_call <- function(call_row, data, lookup) {
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
  parent_children <- rows_by_parent_lookup(parent_id, lookup)

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
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with class metadata or NULL if parsing fails.
#' @keywords internal
extract_r6_class_info <- function(call_row, data, lookup) {
  # The R6Class token's parent is the namespace expr (R6::R6Class)
  # We need the grandparent which contains the full function call with arguments
  namespace_expr_id <- call_row$parent
  namespace_expr <- rows_by_id_lookup(namespace_expr_id, lookup)

  if (nrow(namespace_expr) == 0) {
    return(NULL)
  }

  # Get the grandparent (the full call expression)
  full_call_id <- namespace_expr$parent[1]
  full_call_expr <- rows_by_id_lookup(full_call_id, lookup)

  if (nrow(full_call_expr) == 0) {
    return(NULL)
  }

  # Get all descendants of the full call to find arguments
  descendants <- get_descendants_lookup(full_call_id, lookup)

  # Extract class name (first STR_CONST argument)
  class_name <- extract_class_name_r6(descendants)

  if (is.null(class_name) || !nzchar(class_name)) {
    return(NULL)
  }

  # Extract inheritance information
  inherits_from <- extract_inherit_r6(descendants, data, lookup)

  # Extract public members
  public_members <- extract_members_r6(descendants, data, "public", lookup)

  # Extract private members
  private_members <- extract_members_r6(descendants, data, "private", lookup)

  # Extract active bindings
  active_members <- extract_members_r6(descendants, data, "active", lookup)

  class_name_range <- extract_r6_assignment_range(full_call_expr, lookup) %||%
    extract_r6_string_range(descendants)

  list(
    type = "r6_class",
    class_name = class_name,
    inherits_from = inherits_from,
    public_members = public_members,
    private_members = private_members,
    active_members = active_members,
    member_references = extract_r6_member_references(descendants, lookup),
    range = c(full_call_expr$line1[1], full_call_expr$col1[1], full_call_expr$line2[1], full_call_expr$col2[1]),
    name_range = class_name_range
  )
}

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
#' @param lookup Lookup index from build_lookup_index() for O(1) child lookups.
#' @return Character string of parent class name or NULL.
#' @keywords internal
extract_inherit_r6 <- function(descendants, data, lookup) {
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
    siblings <- rows_by_parent_lookup(eq_parent, lookup)

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
        expr_children <- rows_by_parent_lookup(expr_right$id[1], lookup)
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
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List of member info (name, is_method) or NULL.
#' @keywords internal
extract_members_r6 <- function(descendants, data, member_type, lookup) {
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
    siblings <- rows_by_parent_lookup(eq_parent, lookup)

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
        list_descendants <- get_descendants_lookup(expr_right$id[1], lookup)

        # Extract member info from the list (name + is_method)
        members <- list()

        # Look for named list elements (name = value)
        eq_assign_rows <- list_descendants[list_descendants$token == "EQ_SUB", , drop = FALSE]

        for (j in seq_len(nrow(eq_assign_rows))) {
          eq_assign_row <- eq_assign_rows[j, ]
          eq_assign_parent <- eq_assign_row$parent

          # Get siblings of this assignment
          eq_assign_siblings <- rows_by_parent_lookup(eq_assign_parent, lookup)

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
              rhs_descendants <- get_descendants_lookup(rhs_expr$id[1], lookup)
              is_method <- any(rhs_descendants$token == "FUNCTION")
            }

            members[[length(members) + 1]] <- list(
              name = member_name,
              is_method = is_method,
              name_range = c(
                symbol_lhs$line1[nrow(symbol_lhs)],
                symbol_lhs$col1[nrow(symbol_lhs)],
                symbol_lhs$line2[nrow(symbol_lhs)],
                symbol_lhs$col2[nrow(symbol_lhs)]
              )
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
#' @return List with `symbols` and `occurrences` entries.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_r6.R", relative_path = "my_r6.R"))
#' entries <- build_r6_symbol_entries(ast, "my_r6.R", NULL)
#' }
build_r6_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  r6_classes <- extract_r6_symbols(ast_module)

  if (length(r6_classes) == 0) {
    return(list(symbols = list(), occurrences = list()))
  }

  data <- ast_module$parse_data
  symbols <- list()
  occurrences <- list()
  class_symbols <- new.env(hash = TRUE, parent = emptyenv())
  member_symbols <- new.env(hash = TRUE, parent = emptyenv())

  for (class_info in r6_classes) {
    # Build class symbol ID using the helper from symbols.R
    class_symbol <- build_r6_symbol_id(class_info$class_name, relative_path, namespace)
    class_symbols[[class_info$class_name]] <- class_symbol

    # Add class symbol
    symbols[[length(symbols) + 1]] <- list(
      symbol = class_symbol,
      display_name = class_info$class_name,
      kind = "r6_class",
      documentation = extract_roxygen_documentation(data, class_info$range[1])
    )
    occurrences[[length(occurrences) + 1]] <- r6_occurrence(
      class_symbol,
      class_info$name_range,
      "definition"
    )

    # Process public members (list of {name, is_method} info)
    if (!is.null(class_info$public_members) && length(class_info$public_members) > 0) {
      for (member_info in class_info$public_members) {
        entry <- r6_member_symbol_entry(class_info$class_name, class_symbol, member_info, "public")
        member_symbols[[entry$key]] <- entry$symbol$symbol
        symbols[[length(symbols) + 1]] <- entry$symbol
        occurrences[[length(occurrences) + 1]] <- r6_occurrence(
          entry$symbol$symbol,
          member_info$name_range,
          "definition"
        )
      }
    }

    # Process private members (list of {name, is_method} info)
    if (!is.null(class_info$private_members) && length(class_info$private_members) > 0) {
      for (member_info in class_info$private_members) {
        entry <- r6_member_symbol_entry(class_info$class_name, class_symbol, member_info, "private")
        member_symbols[[entry$key]] <- entry$symbol$symbol
        symbols[[length(symbols) + 1]] <- entry$symbol
        occurrences[[length(occurrences) + 1]] <- r6_occurrence(
          entry$symbol$symbol,
          member_info$name_range,
          "definition"
        )
      }
    }

    # Process active bindings (list of {name, is_method} info, treat as properties)
    if (!is.null(class_info$active_members) && length(class_info$active_members) > 0) {
      for (member_info in class_info$active_members) {
        entry <- r6_member_symbol_entry(class_info$class_name, class_symbol, member_info, "active")
        member_symbols[[entry$key]] <- entry$symbol$symbol
        symbols[[length(symbols) + 1]] <- entry$symbol
        occurrences[[length(occurrences) + 1]] <- r6_occurrence(
          entry$symbol$symbol,
          member_info$name_range,
          "definition"
        )
      }
    }

    for (reference in class_info$member_references) {
      member_symbol <- r6_member_reference_symbol(member_symbols, class_info$class_name, reference)
      if (!is.null(member_symbol)) {
        occurrences[[length(occurrences) + 1]] <- r6_occurrence(
          member_symbol,
          reference$range,
          "reference"
        )
      }
    }
  }

  occurrences <- append_r6_constructor_references(occurrences, data, class_symbols, member_symbols)

  list(symbols = symbols, occurrences = occurrences)
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

  # Build local symbol ID (backtick-quote dotted R names per SCIP spec)
  esc_name <- escape_scip_identifier(name)
  if (!is.null(namespace) && nzchar(namespace)) {
    sprintf("%s %s#%s", namespace, base_path, esc_name)
  } else {
    sprintf("local %s#%s", base_path, esc_name)
  }
}

r6_member_symbol_entry <- function(class_name, class_symbol, member_info, scope) {
  member_name <- member_info$name

  if (identical(scope, "active")) {
    kind <- "r6_active"
    suffix <- "."
  } else if (member_name == "initialize" && identical(scope, "public")) {
    kind <- "r6_constructor"
    suffix <- "()."
  } else if (isTRUE(member_info$is_method)) {
    kind <- "r6_method"
    suffix <- "()."
  } else {
    kind <- "r6_field"
    suffix <- "."
  }

  member_symbol <- paste0(class_symbol, "#", escape_scip_identifier(member_name), suffix)
  list(
    key = r6_member_key(class_name, scope, member_name),
    symbol = list(
      symbol = member_symbol,
      display_name = paste0(class_name, "$", member_name),
      kind = kind,
      documentation = NULL,
      enclosing_symbol = class_symbol
    )
  )
}

r6_member_key <- function(class_name, scope, member_name) {
  paste(class_name, scope, member_name, sep = "\r")
}

r6_member_reference_symbol <- function(member_symbols, class_name, reference) {
  member_symbol <- member_symbols[[r6_member_key(class_name, reference$scope, reference$name)]]
  if (!is.null(member_symbol) || reference$scope != "public") {
    return(member_symbol)
  }

  member_symbols[[r6_member_key(class_name, "active", reference$name)]]
}

r6_occurrence <- function(symbol, range, role) {
  list(
    symbol = symbol,
    range = c(range[1] - 1L, range[2] - 1L, range[3] - 1L, range[4]),
    role = role
  )
}

extract_r6_assignment_range <- function(full_call_expr, lookup) {
  assignment_parent <- rows_by_id_lookup(full_call_expr$parent[1], lookup)
  if (nrow(assignment_parent) == 0) {
    return(NULL)
  }

  siblings <- rows_by_parent_lookup(assignment_parent$id[1], lookup)
  assignment_rows <- siblings[siblings$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), , drop = FALSE]
  if (nrow(assignment_rows) == 0) {
    return(NULL)
  }

  assign_row <- assignment_rows[1, ]
  lhs_exprs <- siblings[siblings$token == "expr" &
    (siblings$line1 < assign_row$line1 |
      (siblings$line1 == assign_row$line1 & siblings$col1 < assign_row$col1)), , drop = FALSE]
  if (nrow(lhs_exprs) == 0) {
    return(NULL)
  }

  lhs_exprs <- lhs_exprs[order(lhs_exprs$line1, lhs_exprs$col1), , drop = FALSE]
  lhs_descendants <- get_descendants_lookup(lhs_exprs$id[nrow(lhs_exprs)], lookup)
  lhs_symbols <- lhs_descendants[lhs_descendants$token == "SYMBOL", , drop = FALSE]
  if (nrow(lhs_symbols) == 0) {
    return(NULL)
  }

  lhs_symbols <- lhs_symbols[order(lhs_symbols$line1, lhs_symbols$col1), , drop = FALSE]
  c(lhs_symbols$line1[1], lhs_symbols$col1[1], lhs_symbols$line2[1], lhs_symbols$col2[1])
}

extract_r6_string_range <- function(descendants) {
  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  if (nrow(str_const_rows) == 0) {
    return(NULL)
  }
  c(str_const_rows$line1[1], str_const_rows$col1[1], str_const_rows$line2[1], str_const_rows$col2[1])
}

extract_r6_member_references <- function(descendants, lookup) {
  candidate_rows <- descendants[descendants$token %in% c("SYMBOL", "SYMBOL_FUNCTION_CALL"), , drop = FALSE]
  references <- list()

  for (i in seq_len(nrow(candidate_rows))) {
    row <- candidate_rows[i, ]
    receiver <- r6_dollar_receiver(row, lookup)
    if (is.null(receiver) || !(receiver %in% c("self", "private"))) {
      next
    }

    references[[length(references) + 1]] <- list(
      scope = if (receiver == "private") "private" else "public",
      name = row$text,
      range = c(row$line1, row$col1, row$line2, row$col2)
    )
  }

  references
}

r6_dollar_receiver <- function(member_row, lookup) {
  siblings <- rows_by_parent_lookup(member_row$parent[1], lookup)
  dollar_rows <- siblings[siblings$token == "'$'", , drop = FALSE]
  if (nrow(dollar_rows) == 0) {
    return(NULL)
  }

  receiver_exprs <- siblings[siblings$token == "expr" &
    (siblings$line1 < dollar_rows$line1[1] |
      (siblings$line1 == dollar_rows$line1[1] & siblings$col1 < dollar_rows$col1[1])), , drop = FALSE]
  if (nrow(receiver_exprs) == 0) {
    return(NULL)
  }

  receiver_exprs <- receiver_exprs[order(receiver_exprs$line1, receiver_exprs$col1), , drop = FALSE]
  receiver_descendants <- get_descendants_lookup(receiver_exprs$id[nrow(receiver_exprs)], lookup)
  receiver_symbols <- receiver_descendants[receiver_descendants$token == "SYMBOL", , drop = FALSE]
  if (nrow(receiver_symbols) == 0) {
    return(NULL)
  }

  receiver_symbols$text[1]
}

append_r6_constructor_references <- function(occurrences, data, class_symbols, member_symbols) {
  if (is.null(data) || nrow(data) == 0) {
    return(occurrences)
  }

  lookup <- build_lookup_index(data)
  new_calls <- data[data$token == "SYMBOL_FUNCTION_CALL" & data$text == "new", , drop = FALSE]
  for (i in seq_len(nrow(new_calls))) {
    new_row <- new_calls[i, ]
    receiver <- r6_dollar_receiver(new_row, lookup)
    if (is.null(receiver)) {
      next
    }

    class_symbol <- class_symbols[[receiver]]
    constructor_symbol <- member_symbols[[r6_member_key(receiver, "public", "initialize")]]
    if (is.null(class_symbol) || is.null(constructor_symbol)) {
      next
    }

    occurrences[[length(occurrences) + 1]] <- r6_occurrence(
      class_symbol,
      c(new_row$line1, new_row$col1 - nchar(receiver) - 1L, new_row$line1, new_row$col1 - 2L),
      "reference"
    )
    occurrences[[length(occurrences) + 1]] <- r6_occurrence(
      constructor_symbol,
      c(new_row$line1, new_row$col1, new_row$line2, new_row$col2),
      "reference"
    )
  }

  occurrences
}
