#' @include parser.R
#' @include parse-tree-utils.R
NULL

# This module detects S4 object-oriented programming patterns in R code.
# It identifies S4 classes (setClass), generics (setGeneric), methods (setMethod),
# and slot definitions. S4 is R's formal OOP system, widely used in Bioconductor.

#' Extract S4 class definitions from an AST module.
#'
#' Scans parse data for setClass() function calls and extracts class metadata
#' including class name, slots, and inheritance (contains).
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @return List of S4 class information structures.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_s4.R", relative_path = "my_s4.R"))
#' s4_classes <- extract_s4_symbols(ast)
#' }
extract_s4_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  s4_symbols <- list()

  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]

  if (nrow(func_calls) == 0) {
    return(list())
  }

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    call_text <- call_row$text

    if (call_text == "setClass") {
      class_info <- extract_setclass_info(call_row, data)
      if (!is.null(class_info)) {
        s4_symbols[[length(s4_symbols) + 1]] <- class_info
      }
    } else if (call_text == "setGeneric") {
      generic_info <- extract_setgeneric_info(call_row, data)
      if (!is.null(generic_info)) {
        s4_symbols[[length(s4_symbols) + 1]] <- generic_info
      }
    } else if (call_text == "setMethod") {
      method_info <- extract_setmethod_info(call_row, data)
      if (!is.null(method_info)) {
        s4_symbols[[length(s4_symbols) + 1]] <- method_info
      }
    }
  }

  s4_symbols
}

#' Extract information from a setClass() call.
#'
#' @param call_row Parse data row for the setClass function call.
#' @param data Full parse data data.frame.
#' @return List with class metadata or NULL if parsing fails.
#' @keywords internal
extract_setclass_info <- function(call_row, data) {
  call_expr_id <- call_row$parent
  call_expr <- data[data$id == call_expr_id, , drop = FALSE]

  if (nrow(call_expr) == 0) {
    return(NULL)
  }

  full_call_id <- call_expr$parent[1]
  descendants <- get_descendants(full_call_id, data)

  class_name <- extract_first_string_arg(descendants)

  if (is.null(class_name) || !nzchar(class_name)) {
    return(NULL)
  }

  slots <- extract_named_list_arg(descendants, data, "slots")
  if (is.null(slots)) {
    slots <- extract_named_list_arg(descendants, data, "representation")
  }

  contains <- extract_string_vector_arg(descendants, data, "contains")

  list(
    type = "s4_class",
    class_name = class_name,
    slots = slots,
    contains = contains,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from a setGeneric() call.
#'
#' @param call_row Parse data row for the setGeneric function call.
#' @param data Full parse data data.frame.
#' @return List with generic metadata or NULL if parsing fails.
#' @keywords internal
extract_setgeneric_info <- function(call_row, data) {
  call_expr_id <- call_row$parent
  call_expr <- data[data$id == call_expr_id, , drop = FALSE]

  if (nrow(call_expr) == 0) {
    return(NULL)
  }

  full_call_id <- call_expr$parent[1]
  descendants <- get_descendants(full_call_id, data)

  generic_name <- extract_first_string_arg(descendants)

  if (is.null(generic_name) || !nzchar(generic_name)) {
    return(NULL)
  }

  list(
    type = "s4_generic",
    generic_name = generic_name,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from a setMethod() call.
#'
#' @param call_row Parse data row for the setMethod function call.
#' @param data Full parse data data.frame.
#' @return List with method metadata or NULL if parsing fails.
#' @keywords internal
extract_setmethod_info <- function(call_row, data) {
  call_expr_id <- call_row$parent
  call_expr <- data[data$id == call_expr_id, , drop = FALSE]

  if (nrow(call_expr) == 0) {
    return(NULL)
  }

  full_call_id <- call_expr$parent[1]
  descendants <- get_descendants(full_call_id, data)

  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  str_const_rows <- str_const_rows[order(str_const_rows$line1, str_const_rows$col1), , drop = FALSE]

  if (nrow(str_const_rows) < 1) {
    return(NULL)
  }

  generic_name <- gsub("^['\"]|['\"]$", "", str_const_rows$text[1])

  signature <- if (nrow(str_const_rows) >= 2) {
    gsub("^['\"]|['\"]$", "", str_const_rows$text[2])
  } else {
    NULL
  }

  list(
    type = "s4_method",
    generic_name = generic_name,
    signature = signature,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract the first string constant argument from descendants.
#'
#' @param descendants Parse data subset.
#' @return Character string or NULL.
#' @keywords internal
extract_first_string_arg <- function(descendants) {
  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]

  if (nrow(str_const_rows) == 0) {
    return(NULL)
  }

  str_const_rows <- str_const_rows[order(str_const_rows$line1, str_const_rows$col1), , drop = FALSE]
  gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
}

#' Extract a named list argument (e.g., slots = c(x = "numeric")).
#'
#' @param descendants Parse data subset.
#' @param data Full parse data.
#' @param arg_name Name of the argument to extract.
#' @return Character vector of slot/member names or NULL.
#' @keywords internal
extract_named_list_arg <- function(descendants, data, arg_name) {
  eq_sub_rows <- descendants[descendants$token == "EQ_SUB", , drop = FALSE]

  if (nrow(eq_sub_rows) == 0) {
    return(NULL)
  }

  for (i in seq_len(nrow(eq_sub_rows))) {
    eq_row <- eq_sub_rows[i, ]
    eq_parent <- eq_row$parent

    siblings <- data[data$parent == eq_parent, , drop = FALSE]

    symbol_left <- siblings[siblings$token == "SYMBOL_SUB" &
                            siblings$line1 == eq_row$line1 &
                            siblings$col1 < eq_row$col1, , drop = FALSE]

    if (nrow(symbol_left) > 0 && any(symbol_left$text == arg_name)) {
      expr_right <- siblings[siblings$token == "expr" &
                             siblings$line1 >= eq_row$line1 &
                             siblings$col1 > eq_row$col1, , drop = FALSE]

      if (nrow(expr_right) > 0) {
        list_descendants <- get_descendants(expr_right$id[1], data)

        slot_names <- character()
        inner_eq <- list_descendants[list_descendants$token == "EQ_SUB", , drop = FALSE]

        for (j in seq_len(nrow(inner_eq))) {
          inner_eq_row <- inner_eq[j, ]
          inner_siblings <- data[data$parent == inner_eq_row$parent, , drop = FALSE]

          slot_symbol <- inner_siblings[inner_siblings$token %in% c("SYMBOL", "SYMBOL_SUB") &
                                        inner_siblings$col1 < inner_eq_row$col1, , drop = FALSE]
          if (nrow(slot_symbol) > 0) {
            slot_symbol <- slot_symbol[order(slot_symbol$col1, decreasing = TRUE), , drop = FALSE]
            slot_names <- c(slot_names, slot_symbol$text[1])
          }
        }

        if (length(slot_names) > 0) {
          return(unique(slot_names))
        }
      }
    }
  }

  NULL
}

#' Extract a string vector argument (e.g., contains = "ParentClass").
#'
#' @param descendants Parse data subset.
#' @param data Full parse data.
#' @param arg_name Name of the argument to extract.
#' @return Character vector of values or NULL.
#' @keywords internal
extract_string_vector_arg <- function(descendants, data, arg_name) {
  eq_sub_rows <- descendants[descendants$token == "EQ_SUB", , drop = FALSE]

  if (nrow(eq_sub_rows) == 0) {
    return(NULL)
  }

  for (i in seq_len(nrow(eq_sub_rows))) {
    eq_row <- eq_sub_rows[i, ]
    eq_parent <- eq_row$parent

    siblings <- data[data$parent == eq_parent, , drop = FALSE]

    symbol_left <- siblings[siblings$token == "SYMBOL_SUB" &
                            siblings$line1 == eq_row$line1 &
                            siblings$col1 < eq_row$col1, , drop = FALSE]

    if (nrow(symbol_left) > 0 && any(symbol_left$text == arg_name)) {
      expr_right <- siblings[siblings$token == "expr" &
                             siblings$col1 > eq_row$col1, , drop = FALSE]

      if (nrow(expr_right) > 0) {
        list_descendants <- get_descendants(expr_right$id[1], data)

        str_rows <- list_descendants[list_descendants$token == "STR_CONST", , drop = FALSE]
        if (nrow(str_rows) > 0) {
          values <- gsub("^['\"]|['\"]$", "", str_rows$text)
          return(unique(values))
        }
      }
    }
  }

  NULL
}

#' Convert S4 symbol info into SCIP-compatible symbol entries.
#'
#' Takes the output of extract_s4_symbols() and converts it into a list of
#' symbol entries suitable for SCIP serialization.
#'
#' @param ast_module Parsed AST module (from parse_file).
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List of symbol entries with symbol, display_name, kind fields.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "my_s4.R", relative_path = "my_s4.R"))
#' symbols <- build_s4_symbol_entries(ast, "my_s4.R", NULL)
#' }
build_s4_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  s4_infos <- extract_s4_symbols(ast_module)

  if (length(s4_infos) == 0) {
    return(list())
  }

  data <- ast_module$parse_data
  symbols <- list()

  for (s4_info in s4_infos) {
    if (s4_info$type == "s4_class") {
      class_symbol <- build_s4_symbol_id(s4_info$class_name, relative_path, namespace)

      symbols[[length(symbols) + 1]] <- list(
        symbol = class_symbol,
        display_name = s4_info$class_name,
        kind = "s4_class",
        documentation = extract_roxygen_documentation(data, s4_info$range[1])
      )

      if (!is.null(s4_info$slots) && length(s4_info$slots) > 0) {
        for (slot_name in s4_info$slots) {
          slot_symbol <- paste0(class_symbol, "@", slot_name, ".")

          symbols[[length(symbols) + 1]] <- list(
            symbol = slot_symbol,
            display_name = slot_name,
            kind = "s4_slot",
            documentation = NULL,
            enclosing_symbol = class_symbol
          )
        }
      }

    } else if (s4_info$type == "s4_generic") {
      symbol_id <- build_s4_symbol_id(s4_info$generic_name, relative_path, namespace)

      symbols[[length(symbols) + 1]] <- list(
        symbol = symbol_id,
        display_name = s4_info$generic_name,
        kind = "s4_generic",
        documentation = extract_roxygen_documentation(data, s4_info$range[1])
      )

    } else if (s4_info$type == "s4_method") {
      method_name <- if (!is.null(s4_info$signature)) {
        paste0(s4_info$generic_name, ",", s4_info$signature)
      } else {
        s4_info$generic_name
      }
      symbol_id <- build_s4_symbol_id(method_name, relative_path, namespace)

      symbols[[length(symbols) + 1]] <- list(
        symbol = symbol_id,
        display_name = method_name,
        kind = "s4_method",
        documentation = extract_roxygen_documentation(data, s4_info$range[1])
      )
    }
  }

  symbols
}

#' Build a SCIP symbol identifier for S4 symbols.
#'
#' Helper function to construct SCIP symbol IDs in the correct format.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_s4_symbol_id <- function(name, relative_path, namespace = NULL) {
  base_path <- gsub("\\.R$", "", relative_path)
  base_path <- gsub("/", "_", base_path)
  base_path <- gsub("-", "_", base_path)

  # Backtick-quote dotted R names per SCIP spec
  esc_name <- escape_scip_identifier(name)
  if (!is.null(namespace) && nzchar(namespace)) {
    sprintf("%s %s#%s", namespace, base_path, esc_name)
  } else {
    sprintf("local %s#%s", base_path, esc_name)
  }
}
