#' @include parser.R
#' @include parse-tree-utils.R
NULL

# This module detects Shiny application patterns in R code.
# It identifies Shiny modules (moduleServer, NS), reactive values,
# observers, and UI/server component definitions.
# Enables navigation and understanding of Shiny application structure.

#' Extract Shiny-specific symbols from an AST module.
#'
#' Scans parse data for Shiny patterns including:
#' - Module definitions (moduleServer, callModule)
#' - Namespace functions (NS)
#' - Reactive values (reactiveVal, reactiveValues)
#' - Observers and outputs (observe, observeEvent, render*)
#'
#' @param ast_module List produced by `parse_file()` containing expressions and parse data.
#' @return List of Shiny symbol information structures.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "app.R", relative_path = "app.R"))
#' shiny_symbols <- extract_shiny_symbols(ast)
#' }
extract_shiny_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }

  shiny_symbols <- list()

  func_calls <- data[data$token == "SYMBOL_FUNCTION_CALL", , drop = FALSE]

  if (nrow(func_calls) == 0) {
    return(list())
  }

  module_functions <- c("moduleServer", "callModule")
  reactive_functions <- c("reactiveVal", "reactiveValues", "reactive")
  observer_functions <- c("observe", "observeEvent", "eventReactive")
  render_pattern <- "^render"

  # Build a lightweight id/parent lookup once per file, but ONLY when the file
  # contains a per-call shiny construct that needs it (module/reactive/observer/
  # render/NS). The per-call extractors below are the only lookup consumers;
  # extract_module_definitions() uses no lookup. Skipping the build on files
  # with no such calls avoids per-file index overhead (identical output).
  call_needs_lookup <- func_calls$text %in%
    c(module_functions, reactive_functions, observer_functions) |
    grepl(render_pattern, func_calls$text) |
    func_calls$text == "NS"
  lookup <- if (any(call_needs_lookup)) build_lookup_index(data) else NULL

  for (i in seq_len(nrow(func_calls))) {
    call_row <- func_calls[i, ]
    call_text <- call_row$text

    if (call_text %in% module_functions) {
      module_info <- extract_module_info(call_row, data, lookup)
      if (!is.null(module_info)) {
        shiny_symbols[[length(shiny_symbols) + 1]] <- module_info
      }
    } else if (call_text %in% reactive_functions) {
      reactive_info <- extract_reactive_info(call_row, data, call_text, lookup)
      if (!is.null(reactive_info)) {
        shiny_symbols[[length(shiny_symbols) + 1]] <- reactive_info
      }
    } else if (call_text %in% observer_functions) {
      observer_info <- extract_observer_info(call_row, data, call_text, lookup)
      if (!is.null(observer_info)) {
        shiny_symbols[[length(shiny_symbols) + 1]] <- observer_info
      }
    } else if (grepl(render_pattern, call_text)) {
      output_info <- extract_render_info(call_row, data, call_text, lookup)
      if (!is.null(output_info)) {
        shiny_symbols[[length(shiny_symbols) + 1]] <- output_info
      }
    } else if (call_text == "NS") {
      ns_info <- extract_ns_info(call_row, data, lookup)
      if (!is.null(ns_info)) {
        shiny_symbols[[length(shiny_symbols) + 1]] <- ns_info
      }
    }
  }

  module_defs <- extract_module_definitions(data)
  shiny_symbols <- c(shiny_symbols, module_defs)

  shiny_symbols
}

#' Extract information from a moduleServer() or callModule() call.
#'
#' @param call_row Parse data row for the function call.
#' @param data Full parse data data.frame.
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with module metadata or NULL if parsing fails.
#' @keywords internal
extract_module_info <- function(call_row, data, lookup) {
  call_expr_id <- call_row$parent
  full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

  if (is.na(full_call_id)) {
    return(NULL)
  }

  descendants <- get_descendants_lookup(full_call_id, lookup)

  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  module_id <- if (nrow(str_const_rows) > 0) {
    gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
  } else {
    symbol_rows <- descendants[descendants$token == "SYMBOL", , drop = FALSE]
    if (nrow(symbol_rows) > 0) {
      symbol_rows$text[1]
    } else {
      NULL
    }
  }

  assigned_name <- find_assigned_name(full_call_id, data, lookup)

  list(
    type = "shiny_module",
    module_id = module_id,
    assigned_name = assigned_name,
    call_type = call_row$text,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from reactive value definitions.
#'
#' @param call_row Parse data row for the function call.
#' @param data Full parse data data.frame.
#' @param call_text The function name (reactiveVal, reactiveValues, etc.).
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with reactive metadata or NULL.
#' @keywords internal
extract_reactive_info <- function(call_row, data, call_text, lookup) {
  call_expr_id <- call_row$parent
  full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

  if (is.na(full_call_id)) {
    full_call_id <- call_expr_id
  }

  assigned_name <- find_assigned_name(full_call_id, data, lookup)

  list(
    type = "shiny_reactive",
    reactive_type = call_text,
    assigned_name = assigned_name,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from observer definitions.
#'
#' @param call_row Parse data row for the function call.
#' @param data Full parse data data.frame.
#' @param call_text The function name (observe, observeEvent, etc.).
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with observer metadata or NULL.
#' @keywords internal
extract_observer_info <- function(call_row, data, call_text, lookup) {
  call_expr_id <- call_row$parent
  full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

  if (is.na(full_call_id)) {
    full_call_id <- call_expr_id
  }

  assigned_name <- find_assigned_name(full_call_id, data, lookup)

  list(
    type = "shiny_observer",
    observer_type = call_text,
    assigned_name = assigned_name,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from render* function calls.
#'
#' @param call_row Parse data row for the function call.
#' @param data Full parse data data.frame.
#' @param call_text The function name (renderPlot, renderTable, etc.).
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with output metadata or NULL.
#' @keywords internal
extract_render_info <- function(call_row, data, call_text, lookup) {
  call_expr_id <- call_row$parent
  full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

  if (is.na(full_call_id)) {
    full_call_id <- call_expr_id
  }

  output_name <- find_output_assignment(full_call_id, data, lookup)

  list(
    type = "shiny_output",
    render_type = call_text,
    output_name = output_name,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Extract information from NS() calls.
#'
#' @param call_row Parse data row for the NS function call.
#' @param data Full parse data data.frame.
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return List with namespace metadata or NULL.
#' @keywords internal
extract_ns_info <- function(call_row, data, lookup) {
  call_expr_id <- call_row$parent
  full_call_id <- rows_by_id_lookup(call_expr_id, lookup)$parent[1]

  if (is.na(full_call_id)) {
    full_call_id <- call_expr_id
  }

  descendants <- get_descendants_lookup(full_call_id, lookup)

  str_const_rows <- descendants[descendants$token == "STR_CONST", , drop = FALSE]
  ns_id <- if (nrow(str_const_rows) > 0) {
    gsub("^['\"]|['\"]$", "", str_const_rows$text[1])
  } else {
    NULL
  }

  list(
    type = "shiny_namespace",
    ns_id = ns_id,
    range = c(call_row$line1, call_row$col1, call_row$line2, call_row$col2)
  )
}

#' Find module UI and Server function definitions.
#'
#' Looks for functions following the naming pattern *UI, *Server, *Input, *Output.
#'
#' @param data Full parse data data.frame.
#' @return List of module definition structures.
#' @keywords internal
extract_module_definitions <- function(data) {
  module_defs <- list()

  func_rows <- data[data$token == "FUNCTION", , drop = FALSE]

  module_suffixes <- c("UI", "Server", "Input", "Output", "Module")

  for (idx in seq_len(nrow(func_rows))) {
    func_row <- func_rows[idx, ]

    symbol_on_line <- data[data$line1 == func_row$line1 &
      data$token == "SYMBOL" &
      data$col1 < func_row$col1, , drop = FALSE]

    if (nrow(symbol_on_line) == 0) next

    symbol_on_line <- symbol_on_line[order(symbol_on_line$col1, decreasing = TRUE), , drop = FALSE]
    func_name <- symbol_on_line$text[1]
    func_symbol_row <- symbol_on_line[1, ]

    for (suffix in module_suffixes) {
      if (endsWith(func_name, suffix)) {
        base_name <- sub(paste0(suffix, "$"), "", func_name)

        module_defs[[length(module_defs) + 1]] <- list(
          type = "shiny_module_function",
          func_name = func_name,
          base_name = base_name,
          component_type = suffix,
          range = c(
            func_symbol_row$line1, func_symbol_row$col1,
            func_symbol_row$line2, func_symbol_row$col2
          )
        )
        break
      }
    }
  }

  module_defs
}

#' Find the variable name assigned to an expression.
#'
#' Navigates up the parse tree to find assignment target.
#'
#' @param expr_id ID of the expression node.
#' @param data Full parse data data.frame.
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return Character name or NULL.
#' @keywords internal
find_assigned_name <- function(expr_id, data, lookup) {
  assignment_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")

  current_id <- expr_id
  max_levels <- 10

  for (level in seq_len(max_levels)) {
    current_row <- rows_by_id_lookup(current_id, lookup)
    if (nrow(current_row) == 0) break

    parent_id <- current_row$parent[1]
    if (is.na(parent_id) || parent_id == 0) {
      siblings <- rows_by_parent_lookup(current_id, lookup)
    } else {
      siblings <- rows_by_parent_lookup(parent_id, lookup)
    }

    assign_rows <- siblings[siblings$token %in% assignment_tokens, , drop = FALSE]
    if (nrow(assign_rows) > 0) {
      all_expr_children <- data[data$parent %in% siblings$id[siblings$token == "expr"], , drop = FALSE]
      symbol_rows <- all_expr_children[all_expr_children$token == "SYMBOL" &
        all_expr_children$col1 < assign_rows$col1[1], , drop = FALSE]

      if (nrow(symbol_rows) == 0) {
        symbol_rows <- siblings[siblings$token == "SYMBOL" &
          siblings$col1 < assign_rows$col1[1], , drop = FALSE]
      }

      if (nrow(symbol_rows) > 0) {
        symbol_rows <- symbol_rows[order(symbol_rows$col1, decreasing = TRUE), , drop = FALSE]
        return(symbol_rows$text[1])
      }
    }

    if (is.na(parent_id) || parent_id == 0) break
    current_id <- parent_id
  }

  NULL
}

#' Find output assignment name for render* calls.
#'
#' Looks for output$name <- render*() pattern.
#'
#' @param expr_id ID of the expression node.
#' @param data Full parse data data.frame.
#' @param lookup Lookup index from build_lookup_index() for O(1) lookups.
#' @return Character output name or NULL.
#' @keywords internal
find_output_assignment <- function(expr_id, data, lookup) {
  current_id <- expr_id
  max_levels <- 5

  for (level in seq_len(max_levels)) {
    current_row <- rows_by_id_lookup(current_id, lookup)
    if (nrow(current_row) == 0) break

    parent_id <- current_row$parent[1]
    if (is.na(parent_id) || parent_id == 0) break

    siblings <- rows_by_parent_lookup(parent_id, lookup)

    dollar_rows <- siblings[siblings$token == "'$'", , drop = FALSE]
    if (nrow(dollar_rows) > 0) {
      output_symbol <- siblings[siblings$token == "SYMBOL" &
        siblings$text == "output", , drop = FALSE]
      if (nrow(output_symbol) > 0) {
        name_symbol <- siblings[siblings$token == "SYMBOL" &
          siblings$col1 > dollar_rows$col1[1], , drop = FALSE]
        if (nrow(name_symbol) > 0) {
          return(name_symbol$text[1])
        }
      }
    }

    current_id <- parent_id
  }

  NULL
}

#' Convert Shiny symbol info into SCIP-compatible symbol entries.
#'
#' Takes the output of extract_shiny_symbols() and converts it into a list of
#' symbol entries suitable for SCIP serialization.
#'
#' @param ast_module Parsed AST module (from parse_file).
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List of symbol entries with symbol, display_name, kind fields.
#' @keywords internal
#' @examples
#' \dontrun{
#' ast <- parse_file(list(path = "app.R", relative_path = "app.R"))
#' symbols <- build_shiny_symbol_entries(ast, "app.R", NULL)
#' }
build_shiny_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  shiny_infos <- extract_shiny_symbols(ast_module)

  if (length(shiny_infos) == 0) {
    return(list())
  }

  data <- ast_module$parse_data
  symbols <- list()

  for (shiny_info in shiny_infos) {
    symbol_name <- NULL
    display_name <- NULL
    kind <- NULL

    if (shiny_info$type == "shiny_module") {
      symbol_name <- shiny_info$assigned_name %||% shiny_info$module_id %||% "anonymous_module"
      display_name <- symbol_name
      kind <- "shiny_module"
    } else if (shiny_info$type == "shiny_module_function") {
      symbol_name <- shiny_info$func_name
      display_name <- shiny_info$func_name
      kind <- if (shiny_info$component_type == "UI") {
        "shiny_ui"
      } else if (shiny_info$component_type == "Server") {
        "shiny_server"
      } else {
        "shiny_module_function"
      }
    } else if (shiny_info$type == "shiny_reactive") {
      symbol_name <- shiny_info$assigned_name %||% "anonymous_reactive"
      display_name <- symbol_name
      kind <- "shiny_reactive"
    } else if (shiny_info$type == "shiny_observer") {
      symbol_name <- shiny_info$assigned_name %||% "anonymous_observer"
      display_name <- symbol_name
      kind <- "shiny_observer"
    } else if (shiny_info$type == "shiny_output") {
      symbol_name <- shiny_info$output_name %||% "anonymous_output"
      display_name <- symbol_name
      kind <- "shiny_output"
    } else if (shiny_info$type == "shiny_namespace") {
      symbol_name <- shiny_info$ns_id %||% "anonymous_ns"
      display_name <- symbol_name
      kind <- "shiny_namespace"
    } else {
      next
    }

    symbol_id <- build_shiny_symbol_id(symbol_name, relative_path, namespace)

    symbols[[length(symbols) + 1]] <- list(
      symbol = symbol_id,
      display_name = display_name,
      kind = kind,
      documentation = extract_roxygen_documentation(data, shiny_info$range[1])
    )
  }

  symbols
}

#' Build a SCIP symbol identifier for Shiny symbols.
#'
#' Helper function to construct SCIP symbol IDs in the correct format.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_shiny_symbol_id <- function(name, relative_path, namespace = NULL) {
  if (!is.null(namespace) && nzchar(namespace)) {
    # For namespaced (package-exported) symbols use the package-level format
    esc_name <- escape_scip_identifier(name)
    sanitized_path <- gsub("[^A-Za-z0-9_/]", "_", relative_path)
    sprintf("%s %s#%s", namespace, sanitized_path, esc_name)
  } else {
    # For local symbols delegate to the canonical helper so the path format
    # matches the variable symbols emitted by the main resolver — this ensures
    # semquery can locate the symbol using the file path.
    build_local_symbol_id(name, relative_path)
  }
}
