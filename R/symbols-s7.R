# S7 symbol extraction for object-oriented R code.
# Detects classes, properties, constructors, generics, and methods for SCIP.
# Inventory: `extract_s7_symbols()`, `build_s7_symbol_entries()`,
# `build_s7_symbol_id()`.

#' @include parser.R
#' @include parse-tree-utils.R
#' @include parse-lookup.R
NULL

S7_FUNCTIONS <- c("new_class", "new_generic", "method", "S7_inherits", "prop", "set_props", "new_property")

#' Extract S7 definitions and deterministic reference sites from an AST module.
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @return List with `definitions` and `references`.
#' @keywords internal
extract_s7_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list(definitions = list(), references = list()))
  }

  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]
  if (nrow(func_calls) == 0 || !any(func_calls$text %in% S7_FUNCTIONS)) {
    return(list(definitions = list(), references = list()))
  }

  lookup <- build_lookup_index(data)
  definitions <- list()
  references <- list()

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    if (is_s7_call(call_row, "new_class", lookup)) {
      class_info <- extract_new_class_info(call_row, lookup)
      if (!is.null(class_info)) {
        definitions[[length(definitions) + 1]] <- class_info
      }
    } else if (is_s7_call(call_row, "new_generic", lookup)) {
      generic_info <- extract_new_generic_info(call_row, lookup)
      if (!is.null(generic_info)) {
        definitions[[length(definitions) + 1]] <- generic_info
      }
    } else if (is_s7_call(call_row, "method", lookup)) {
      method_info <- extract_s7_method_info(call_row, lookup)
      if (!is.null(method_info)) {
        definitions[[length(definitions) + 1]] <- method_info
      }
    } else if (is_s7_call(call_row, "S7_inherits", lookup)) {
      inherit_ref <- extract_s7_inherits_reference(call_row, lookup)
      if (!is.null(inherit_ref)) {
        references[[length(references) + 1]] <- inherit_ref
      }
    }
  }

  list(definitions = definitions, references = references)
}

#' Check whether a function call row is an S7 call.
#'
#' @param call_row Parse data row for a SYMBOL_FUNCTION_CALL token.
#' @param function_name S7 function name to match.
#' @param lookup Lookup index from build_lookup_index().
#' @return Logical.
#' @keywords internal
is_s7_call <- function(call_row, function_name, lookup) {
  if (!identical(call_row$text, function_name)) {
    return(FALSE)
  }

  parent_children <- rows_by_parent_lookup(call_row$parent, lookup)
  has_ns_operator <- any(parent_children$token %in% c("NS_GET", "NS_GET_INT"))
  if (!has_ns_operator) {
    return(TRUE)
  }

  pkg_rows <- parent_children[parent_children$token == "SYMBOL_PACKAGE", , drop = FALSE]
  nrow(pkg_rows) > 0 && any(pkg_rows$text == "S7")
}

#' Extract information from S7::new_class().
#'
#' @param call_row Parse data row for the new_class function call.
#' @param lookup Lookup index from build_lookup_index().
#' @return List with class metadata or NULL.
#' @keywords internal
extract_new_class_info <- function(call_row, lookup) {
  call_expr_id <- s7_full_call_id(call_row, lookup)
  if (is.null(call_expr_id)) {
    return(NULL)
  }

  descendants <- get_descendants_lookup(call_expr_id, lookup)
  class_name <- s7_first_string(descendants)
  assignment <- s7_assignment_lhs(call_expr_id, lookup)
  if (is.null(class_name) || !nzchar(class_name)) {
    class_name <- assignment$name
  }
  if (is.null(class_name) || !nzchar(class_name)) {
    return(NULL)
  }

  properties <- extract_s7_properties(call_expr_id, lookup)
  constructor <- extract_s7_named_arg(call_expr_id, "constructor", lookup)

  list(
    type = "s7_class",
    class_name = class_name,
    properties = properties,
    has_constructor = !is.null(constructor),
    name_range = assignment$range %||% s7_row_range(call_row),
    constructor_range = if (!is.null(constructor)) constructor$name_range else NULL,
    range = s7_row_range(call_row)
  )
}

#' Extract information from S7::new_generic().
#'
#' @param call_row Parse data row for the new_generic function call.
#' @param lookup Lookup index from build_lookup_index().
#' @return List with generic metadata or NULL.
#' @keywords internal
extract_new_generic_info <- function(call_row, lookup) {
  call_expr_id <- s7_full_call_id(call_row, lookup)
  if (is.null(call_expr_id)) {
    return(NULL)
  }

  descendants <- get_descendants_lookup(call_expr_id, lookup)
  generic_name <- s7_first_string(descendants)
  dispatch_arg <- s7_nth_string(descendants, 2)
  assignment <- s7_assignment_lhs(call_expr_id, lookup)
  if (is.null(generic_name) || !nzchar(generic_name)) {
    generic_name <- assignment$name
  }
  if (is.null(generic_name) || !nzchar(generic_name)) {
    return(NULL)
  }

  list(
    type = "s7_generic",
    generic_name = generic_name,
    dispatch_arg = dispatch_arg,
    name_range = assignment$range %||% s7_row_range(call_row),
    range = s7_row_range(call_row)
  )
}

#' Extract information from S7::method(generic, Class) <- function(...).
#'
#' @param call_row Parse data row for the method function call.
#' @param lookup Lookup index from build_lookup_index().
#' @return List with method metadata or NULL.
#' @keywords internal
extract_s7_method_info <- function(call_row, lookup) {
  call_expr_id <- s7_full_call_id(call_row, lookup)
  if (is.null(call_expr_id)) {
    return(NULL)
  }

  args <- s7_direct_call_args(call_expr_id, lookup)
  symbol_args <- args[args$token == "SYMBOL", , drop = FALSE]
  if (nrow(symbol_args) < 2) {
    return(NULL)
  }

  symbol_args <- symbol_args[order(symbol_args$line1, symbol_args$col1), , drop = FALSE]
  function_expr <- s7_assignment_rhs(call_expr_id, lookup)
  formal_name <- s7_first_formal(function_expr, lookup)

  list(
    type = "s7_method",
    generic_name = symbol_args$text[1],
    class_name = symbol_args$text[2],
    method_name = paste0(symbol_args$text[1], ",", symbol_args$text[2]),
    generic_range = s7_row_range(symbol_args[1, ]),
    class_range = s7_row_range(symbol_args[2, ]),
    formal_name = formal_name,
    property_refs = extract_s7_method_property_refs(function_expr, formal_name, lookup),
    range = s7_row_range(call_row)
  )
}

#' Convert S7 extraction output into SCIP-compatible symbol entries and occurrences.
#'
#' @param ast_module Parsed AST module.
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List with `symbols` and `occurrences`.
#' @keywords internal
build_s7_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  s7 <- extract_s7_symbols(ast_module)
  if (length(s7$definitions) == 0 && length(s7$references) == 0) {
    return(list(symbols = list(), occurrences = list()))
  }

  data <- ast_module$parse_data
  symbols <- list()
  occurrences <- list()
  class_symbols <- new.env(hash = TRUE, parent = emptyenv())
  generic_symbols <- new.env(hash = TRUE, parent = emptyenv())
  property_symbols <- new.env(hash = TRUE, parent = emptyenv())

  for (s7_info in s7$definitions) {
    if (s7_info$type == "s7_class") {
      class_symbol <- build_s7_symbol_id(s7_info$class_name, relative_path, namespace)
      class_symbols[[s7_info$class_name]] <- class_symbol

      symbols[[length(symbols) + 1]] <- list(
        symbol = class_symbol,
        display_name = s7_info$class_name,
        kind = "s7_class",
        documentation = extract_roxygen_documentation(data, s7_info$range[1])
      )
      occurrences[[length(occurrences) + 1]] <- s7_occurrence(class_symbol, s7_info$name_range, "definition")

      if (isTRUE(s7_info$has_constructor)) {
        constructor_symbol <- paste0(class_symbol, "#initialize().")
        symbols[[length(symbols) + 1]] <- list(
          symbol = constructor_symbol,
          display_name = paste0(s7_info$class_name, "()"),
          kind = "s7_constructor",
          documentation = NULL,
          enclosing_symbol = class_symbol
        )
        occurrences[[length(occurrences) + 1]] <- s7_occurrence(
          constructor_symbol,
          s7_info$constructor_range,
          "definition"
        )
      }

      for (property in s7_info$properties) {
        property_symbol <- paste0(class_symbol, "#", escape_scip_identifier(property$name), ".")
        property_symbols[[paste0(s7_info$class_name, "$", property$name)]] <- property_symbol
        symbols[[length(symbols) + 1]] <- list(
          symbol = property_symbol,
          display_name = property$name,
          kind = "s7_property",
          documentation = NULL,
          enclosing_symbol = class_symbol
        )
        occurrences[[length(occurrences) + 1]] <- s7_occurrence(
          property_symbol,
          property$name_range,
          "definition"
        )
        if (!is.null(property$type_class)) {
          type_symbol <- class_symbols[[property$type_class]]
          if (!is.null(type_symbol)) {
            occurrences[[length(occurrences) + 1]] <- s7_occurrence(
              type_symbol,
              property$type_range,
              "reference"
            )
          }
        }
      }
    } else if (s7_info$type == "s7_generic") {
      generic_symbol <- build_s7_symbol_id(s7_info$generic_name, relative_path, namespace)
      generic_symbols[[s7_info$generic_name]] <- generic_symbol
      symbols[[length(symbols) + 1]] <- list(
        symbol = generic_symbol,
        display_name = s7_info$generic_name,
        kind = "s7_generic",
        documentation = extract_roxygen_documentation(data, s7_info$range[1])
      )
      occurrences[[length(occurrences) + 1]] <- s7_occurrence(generic_symbol, s7_info$name_range, "definition")
    }
  }

  for (s7_info in s7$definitions) {
    if (s7_info$type != "s7_method") {
      next
    }

    method_symbol <- build_s7_symbol_id(s7_info$method_name, relative_path, namespace)
    symbols[[length(symbols) + 1]] <- list(
      symbol = method_symbol,
      display_name = s7_info$method_name,
      kind = "s7_method",
      documentation = extract_roxygen_documentation(data, s7_info$range[1])
    )
    occurrences[[length(occurrences) + 1]] <- s7_occurrence(method_symbol, s7_info$range, "definition")

    generic_symbol <- generic_symbols[[s7_info$generic_name]]
    if (!is.null(generic_symbol)) {
      occurrences[[length(occurrences) + 1]] <- s7_occurrence(generic_symbol, s7_info$generic_range, "reference")
    }

    class_symbol <- class_symbols[[s7_info$class_name]]
    if (!is.null(class_symbol)) {
      occurrences[[length(occurrences) + 1]] <- s7_occurrence(class_symbol, s7_info$class_range, "reference")
    }

    for (property_ref in s7_info$property_refs) {
      property_symbol <- property_symbols[[paste0(s7_info$class_name, "$", property_ref$name)]]
      if (!is.null(property_symbol)) {
        occurrences[[length(occurrences) + 1]] <- s7_occurrence(property_symbol, property_ref$range, "reference")
      }
    }
  }

  for (reference in s7$references) {
    class_symbol <- class_symbols[[reference$class_name]]
    if (!is.null(class_symbol)) {
      occurrences[[length(occurrences) + 1]] <- s7_occurrence(class_symbol, reference$range, "reference")
    }
  }

  list(symbols = symbols, occurrences = occurrences)
}

#' Build a SCIP symbol identifier for S7 symbols.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_s7_symbol_id <- function(name, relative_path, namespace = NULL) {
  base_path <- gsub("\\.R$", "", relative_path)
  base_path <- gsub("/", "_", base_path)
  base_path <- gsub("-", "_", base_path)

  esc_name <- escape_scip_identifier(name)
  if (!is.null(namespace) && nzchar(namespace)) {
    sprintf("%s %s#%s", namespace, base_path, esc_name)
  } else {
    sprintf("local %s#%s", base_path, esc_name)
  }
}

s7_full_call_id <- function(call_row, lookup) {
  namespace_expr <- rows_by_id_lookup(call_row$parent, lookup)
  if (nrow(namespace_expr) == 0) {
    return(NULL)
  }
  namespace_expr$parent[1]
}

s7_direct_call_args <- function(call_expr_id, lookup) {
  children <- rows_by_parent_lookup(call_expr_id, lookup)
  expr_children <- children[children$token == "expr", , drop = FALSE]
  if (nrow(expr_children) <= 1) {
    return(expr_children[integer(0), , drop = FALSE])
  }
  arg_exprs <- expr_children[order(expr_children$line1, expr_children$col1), , drop = FALSE]
  arg_exprs <- arg_exprs[-1, , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(arg_exprs))) {
    descendants <- get_descendants_lookup(arg_exprs$id[i], lookup)
    leaf <- descendants[descendants$token %in% c("SYMBOL", "STR_CONST", "SYMBOL_SUB"), , drop = FALSE]
    if (nrow(leaf) > 0) {
      rows[[length(rows) + 1]] <- leaf[1, , drop = FALSE]
    }
  }
  if (length(rows) == 0) {
    return(children[integer(0), , drop = FALSE])
  }
  do.call(rbind, rows)
}

s7_assignment_lhs <- function(call_expr_id, lookup) {
  call_expr <- rows_by_id_lookup(call_expr_id, lookup)
  if (nrow(call_expr) == 0) {
    return(list(name = NULL, range = NULL))
  }
  parent_children <- rows_by_parent_lookup(call_expr$parent[1], lookup)
  assign_rows <- parent_children[parent_children$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), , drop = FALSE]
  if (nrow(assign_rows) == 0) {
    return(list(name = NULL, range = NULL))
  }
  lhs <- parent_children[parent_children$token == "expr" & parent_children$col1 < assign_rows$col1[1], , drop = FALSE]
  if (nrow(lhs) == 0) {
    return(list(name = NULL, range = NULL))
  }
  lhs <- lhs[order(lhs$col1, decreasing = TRUE), , drop = FALSE]
  descendants <- get_descendants_lookup(lhs$id[1], lookup)
  symbols <- descendants[descendants$token == "SYMBOL", , drop = FALSE]
  if (nrow(symbols) == 0) {
    return(list(name = NULL, range = NULL))
  }
  symbols <- symbols[order(symbols$line1, symbols$col1), , drop = FALSE]
  list(name = symbols$text[1], range = s7_row_range(symbols[1, ]))
}

s7_assignment_rhs <- function(call_expr_id, lookup) {
  call_expr <- rows_by_id_lookup(call_expr_id, lookup)
  if (nrow(call_expr) == 0) {
    return(NULL)
  }
  parent_children <- rows_by_parent_lookup(call_expr$parent[1], lookup)
  assign_rows <- parent_children[parent_children$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN"), , drop = FALSE]
  if (nrow(assign_rows) == 0) {
    return(NULL)
  }
  rhs <- parent_children[parent_children$token == "expr" & parent_children$col1 > assign_rows$col1[1], , drop = FALSE]
  if (nrow(rhs) == 0) {
    return(NULL)
  }
  rhs[order(rhs$col1), , drop = FALSE][1, ]
}

extract_s7_named_arg <- function(call_expr_id, arg_name, lookup) {
  children <- rows_by_parent_lookup(call_expr_id, lookup)
  arg_rows <- children[children$token == "SYMBOL_SUB" & children$text == arg_name, , drop = FALSE]
  if (nrow(arg_rows) == 0) {
    return(NULL)
  }
  arg_row <- arg_rows[1, ]
  eq_rows <- children[
    children$token == "EQ_SUB" & children$line1 == arg_row$line1 & children$col1 > arg_row$col1, ,
    drop = FALSE
  ]
  if (nrow(eq_rows) == 0) {
    return(NULL)
  }
  value_rows <- children[
    children$token == "expr" &
      (children$line1 > eq_rows$line1[1] |
        (children$line1 == eq_rows$line1[1] & children$col1 > eq_rows$col1[1])),
    ,
    drop = FALSE
  ]
  if (nrow(value_rows) == 0) {
    return(NULL)
  }
  value_rows <- value_rows[order(value_rows$line1, value_rows$col1), , drop = FALSE]
  list(name_range = s7_row_range(arg_row), expr = value_rows[1, ])
}

extract_s7_properties <- function(call_expr_id, lookup) {
  properties_arg <- extract_s7_named_arg(call_expr_id, "properties", lookup)
  if (is.null(properties_arg)) {
    return(list())
  }

  children <- rows_by_parent_lookup(properties_arg$expr$id[1], lookup)
  eq_rows <- children[children$token == "EQ_SUB", , drop = FALSE]
  properties <- list()

  for (i in seq_len(nrow(eq_rows))) {
    eq_row <- eq_rows[i, ]
    siblings <- rows_by_parent_lookup(eq_row$parent, lookup)
    property_name <- siblings[
      siblings$token == "SYMBOL_SUB" &
        siblings$line1 == eq_row$line1 &
        siblings$col1 < eq_row$col1,
      ,
      drop = FALSE
    ]
    if (nrow(property_name) == 0) {
      next
    }
    property_name <- property_name[order(property_name$col1, decreasing = TRUE), , drop = FALSE][1, ]
    value_expr <- siblings[
      siblings$token == "expr" &
        (siblings$line1 > eq_row$line1 |
          (siblings$line1 == eq_row$line1 & siblings$col1 > eq_row$col1)),
      ,
      drop = FALSE
    ]
    property_type <- if (nrow(value_expr) > 0) {
      extract_s7_new_property_type(value_expr[order(value_expr$line1, value_expr$col1), , drop = FALSE][1, ], lookup)
    } else {
      NULL
    }
    properties[[length(properties) + 1]] <- list(
      name = property_name$text,
      name_range = s7_row_range(property_name),
      type_class = property_type$name,
      type_range = property_type$range
    )
  }

  properties
}

extract_s7_new_property_type <- function(value_expr, lookup) {
  descendants <- get_descendants_lookup(value_expr$id[1], lookup)
  new_property_calls <- descendants[
    descendants$token == "SYMBOL_FUNCTION_CALL" & descendants$text == "new_property",
    ,
    drop = FALSE
  ]
  if (nrow(new_property_calls) == 0 || !is_s7_call(new_property_calls[1, ], "new_property", lookup)) {
    return(list(name = NULL, range = NULL))
  }

  call_expr_id <- s7_full_call_id(new_property_calls[1, ], lookup)
  args <- s7_direct_call_args(call_expr_id, lookup)
  symbol_args <- args[args$token == "SYMBOL", , drop = FALSE]
  if (nrow(symbol_args) == 0) {
    return(list(name = NULL, range = NULL))
  }
  symbol_args <- symbol_args[order(symbol_args$line1, symbol_args$col1), , drop = FALSE]
  list(name = symbol_args$text[1], range = s7_row_range(symbol_args[1, ]))
}

extract_s7_inherits_reference <- function(call_row, lookup) {
  call_expr_id <- s7_full_call_id(call_row, lookup)
  if (is.null(call_expr_id)) {
    return(NULL)
  }
  args <- s7_direct_call_args(call_expr_id, lookup)
  symbol_args <- args[args$token == "SYMBOL", , drop = FALSE]
  if (nrow(symbol_args) < 2) {
    return(NULL)
  }
  symbol_args <- symbol_args[order(symbol_args$line1, symbol_args$col1), , drop = FALSE]
  list(class_name = symbol_args$text[2], range = s7_row_range(symbol_args[2, ]))
}

s7_first_formal <- function(function_expr, lookup) {
  if (is.null(function_expr)) {
    return(NULL)
  }
  descendants <- get_descendants_lookup(function_expr$id[1], lookup)
  formals <- descendants[descendants$token == "SYMBOL_FORMALS", , drop = FALSE]
  if (nrow(formals) == 0) {
    return(NULL)
  }
  formals <- formals[order(formals$line1, formals$col1), , drop = FALSE]
  formals$text[1]
}

extract_s7_method_property_refs <- function(function_expr, formal_name, lookup) {
  if (is.null(function_expr) || is.null(formal_name) || !nzchar(formal_name)) {
    return(list())
  }
  descendants <- get_descendants_lookup(function_expr$id[1], lookup)
  refs <- list()

  prop_calls <- descendants[descendants$token == "SYMBOL_FUNCTION_CALL" & descendants$text == "prop", , drop = FALSE]
  for (i in seq_len(nrow(prop_calls))) {
    if (!is_s7_call(prop_calls[i, ], "prop", lookup)) {
      next
    }
    call_expr_id <- s7_full_call_id(prop_calls[i, ], lookup)
    args <- s7_direct_call_args(call_expr_id, lookup)
    if (nrow(args) >= 2 && identical(args$text[1], formal_name) && args$token[2] == "STR_CONST") {
      refs[[length(refs) + 1]] <- list(name = s7_unquote(args$text[2]), range = s7_row_range(args[2, ]))
    }
  }

  set_props_calls <- descendants[
    descendants$token == "SYMBOL_FUNCTION_CALL" & descendants$text == "set_props",
    ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(set_props_calls))) {
    if (!is_s7_call(set_props_calls[i, ], "set_props", lookup)) {
      next
    }
    call_expr_id <- s7_full_call_id(set_props_calls[i, ], lookup)
    args <- s7_direct_call_args(call_expr_id, lookup)
    if (nrow(args) < 1 || !identical(args$text[1], formal_name)) {
      next
    }
    call_desc <- get_descendants_lookup(call_expr_id, lookup)
    property_names <- call_desc[call_desc$token == "SYMBOL_SUB", , drop = FALSE]
    property_names <- property_names[!(property_names$text %in% "set_props"), , drop = FALSE]
    for (j in seq_len(nrow(property_names))) {
      refs[[length(refs) + 1]] <- list(
        name = property_names$text[j],
        range = s7_row_range(property_names[j, ])
      )
    }
  }

  slot_rows <- descendants[descendants$token == "SLOT", , drop = FALSE]
  for (i in seq_len(nrow(slot_rows))) {
    parent_children <- rows_by_parent_lookup(slot_rows$parent[i], lookup)
    object_rows <- parent_children[
      parent_children$token == "expr" & parent_children$col1 < slot_rows$col1[i], ,
      drop = FALSE
    ]
    if (nrow(object_rows) == 0) {
      next
    }
    object_desc <- get_descendants_lookup(object_rows$id[1], lookup)
    object_symbols <- object_desc[object_desc$token == "SYMBOL", , drop = FALSE]
    if (nrow(object_symbols) > 0 && identical(object_symbols$text[1], formal_name)) {
      refs[[length(refs) + 1]] <- list(name = slot_rows$text[i], range = s7_row_range(slot_rows[i, ]))
    }
  }

  refs
}

s7_first_string <- function(descendants) {
  s7_nth_string(descendants, 1)
}

s7_nth_string <- function(descendants, n) {
  str_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  if (nrow(str_rows) < n) {
    return(NULL)
  }
  str_rows <- str_rows[order(str_rows$line1, str_rows$col1), , drop = FALSE]
  s7_unquote(str_rows$text[n])
}

s7_unquote <- function(value) {
  gsub("^['\"]|['\"]$", "", value)
}

s7_row_range <- function(row) {
  c(row$line1[1], row$col1[1], row$line2[1], row$col2[1])
}

s7_occurrence <- function(symbol, range, role) {
  list(
    symbol = symbol,
    range = c(range[1] - 1L, range[2] - 1L, range[3] - 1L, range[4]),
    role = role
  )
}
