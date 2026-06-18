#' @include parser.R
#' @include parse-lookup.R
NULL

# This module detects plumber REST API route handlers in R source files.
# Plumber routes are declared as a block of `#*` annotation comments followed
# by a bare, anonymous `function(...)` expression with no name binding, e.g.:
#
#   #* Get gene metadata for a dataset
#   #* @tag data
#   #* @get /dataset/<id>
#   #* @serializer unboxedJSON
#   function(id, res) { ... }
#
# Because the handler is anonymous, the main resolver emits no symbol or
# occurrence for it, so the entire REST API surface is invisible to SCIP.
# This extractor synthesizes a navigable symbol from the HTTP method + route
# path (e.g. `GET_dataset_id`), emits a definition occurrence at the
# `function` keyword, and folds the full annotation block into documentation.

# HTTP method annotations that mark a block as a route handler.
PLUMBER_METHODS <- c("get", "post", "put", "delete", "patch", "head")

# Max line gap allowed between the end of a `#*` block and the handler
# `function` keyword. Plumber requires the function immediately after the
# annotation block; a small slack tolerates a stray blank line while
# preventing a dangling block from binding to a distant, unrelated function.
PLUMBER_MAX_GAP <- 3L

#' Cheap presence gate for plumber annotation blocks.
#'
#' Uses a vectorized `startsWith` (compiled C, no regex engine) over the
#' COMMENT subset so non-plumber files — the overwhelming majority — pay
#' essentially nothing during a whole-workspace reindex.
#'
#' @param data Parse data data.frame from getParseData().
#' @return TRUE if any `#*` comment is present, FALSE otherwise.
#' @keywords internal
plumber_present <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(FALSE)
  }
  is_comment <- data$token == "COMMENT"
  if (!any(is_comment)) {
    return(FALSE)
  }
  any(startsWith(data$text[is_comment], "#*"))
}

#' Synthesize a SCIP symbol name from an HTTP method and route path.
#'
#' `GET /dataset/<id>` becomes `GET_dataset_id`; `GET /` becomes `GET_root`.
#' Path parameters keep their name (angle brackets stripped) so the symbol
#' stays readable and searchable.
#'
#' @param method Upper-case HTTP method (e.g. "GET").
#' @param path Route path (e.g. "/dataset/<id>").
#' @return Synthesized symbol name (character scalar).
#' @keywords internal
synthesize_plumber_name <- function(method, path) {
  p <- sub("^/", "", path)            # drop leading slash
  p <- gsub("[<>]", "", p)            # keep param names, drop angle brackets
  p <- gsub("[^A-Za-z0-9]+", "_", p)  # path separators / specials -> underscore
  p <- gsub("_+", "_", p)             # collapse runs
  p <- gsub("^_|_$", "", p)           # trim edges
  if (!nzchar(p)) {
    p <- "root"
  }
  paste0(method, "_", p)
}

#' Determine whether a handler function is the target of an assignment.
#'
#' Plumber route handlers are idiomatically anonymous. When one is instead
#' assigned to a name (`h <- function(...)`), the main resolver already emits a
#' navigable symbol for it, so the plumber extractor should not synthesize a
#' duplicate. Detection: the FUNCTION token's enclosing expression sits beside
#' an assignment operator under a shared parent.
#'
#' @param func_row Parse data row for the handler's FUNCTION keyword token.
#' @param data Full parse data data.frame.
#' @return TRUE if the handler is assigned to a name, FALSE otherwise.
#' @keywords internal
plumber_handler_is_assigned <- function(func_row, data) {
  assign_tokens <- c("LEFT_ASSIGN", "EQ_ASSIGN", "RIGHT_ASSIGN")

  expr_id <- func_row$parent
  if (is.na(expr_id)) {
    return(FALSE)
  }
  expr_row <- data[data$id == expr_id, , drop = FALSE]
  if (nrow(expr_row) == 0) {
    return(FALSE)
  }

  grandparent <- expr_row$parent[1]
  if (is.na(grandparent) || grandparent == 0) {
    return(FALSE)
  }

  siblings <- data[data$parent == grandparent, , drop = FALSE]
  any(siblings$token %in% assign_tokens)
}

#' Extract plumber route handler information from an AST module.
#'
#' Scans contiguous `#*` comment blocks, reads the HTTP method + route path
#' from each block, and binds it to the nearest following `function` keyword
#' by line proximity. Line-proximity correlation is robust to nesting: it only
#' considers functions that follow a `#*` block, so nested helper closures in a
#' handler body (which carry no `#*` block) are never mistaken for routes.
#'
#' Pure `parse_data` — no source-line reads — so it adds no I/O to a reindex.
#'
#' @param ast_module List produced by `parse_file()` containing parse data.
#' @return List of route info structures (name, method, path, doc, func_row).
#' @keywords internal
extract_plumber_symbols <- function(ast_module) {
  data <- ast_module$parse_data
  if (!plumber_present(data)) {
    return(list())
  }

  # Plumber comment rows, in source order.
  comment_rows <- data[data$token == "COMMENT", , drop = FALSE]
  pc <- comment_rows[startsWith(comment_rows$text, "#*"), , drop = FALSE]
  pc <- pc[order(pc$line1), , drop = FALSE]

  # Group contiguous comment lines into blocks (a line gap > 1 ends a block).
  group <- cumsum(c(TRUE, diff(pc$line1) != 1))

  # FUNCTION keyword rows, sorted by line for nearest-after lookup.
  func_rows <- data[data$token == "FUNCTION", , drop = FALSE]
  func_rows <- func_rows[order(func_rows$line1), , drop = FALSE]

  method_re <- sprintf(
    "^#\\*\\s*@(%s)\\s+(\\S+)", paste(PLUMBER_METHODS, collapse = "|")
  )

  results <- list()
  for (g in unique(group)) {
    block <- pc[group == g, , drop = FALSE]
    block_end <- max(block$line2)

    matches <- regmatches(
      block$text,
      regexec(method_re, block$text, ignore.case = TRUE, perl = TRUE)
    )
    hit <- which(vapply(matches, length, integer(1)) == 3L)
    if (length(hit) == 0) {
      # A `#*` block with no method annotation (e.g. @filter-only) is not a
      # route in this pass; skip it rather than emit a nameless symbol.
      next
    }

    method <- toupper(matches[[hit[1]]][2])
    path <- matches[[hit[1]]][3]

    # Bind to the nearest function keyword that starts after the block.
    candidates <- func_rows[func_rows$line1 > block_end, , drop = FALSE]
    if (nrow(candidates) == 0) {
      next
    }
    func_row <- candidates[1, ]
    if (func_row$line1 - block_end > PLUMBER_MAX_GAP) {
      next
    }

    # A named handler (`h <- function(...)` after the block) already gets a
    # navigable symbol from the main resolver; skip synthesis to avoid emitting
    # a second symbol for the same definition. Idiomatic plumber handlers are
    # anonymous, so this only fires on the non-idiomatic assigned form.
    if (plumber_handler_is_assigned(func_row, data)) {
      next
    }

    doc_lines <- c(
      paste(method, path),
      sub("^#\\*\\s*", "", block$text)
    )

    results[[length(results) + 1]] <- list(
      name = synthesize_plumber_name(method, path),
      method = method,
      path = path,
      doc = doc_lines,
      func_row = func_row
    )
  }

  results
}

#' Convert plumber route info into SCIP symbol entries and occurrences.
#'
#' Unlike the Shiny/S3/S4/R6 builders — whose symbols are named and therefore
#' already carry a definition occurrence from the main resolver — plumber
#' handlers are anonymous, so this builder must emit BOTH a SymbolInformation
#' entry and a definition occurrence. The occurrence range points at the
#' `function` keyword (go-to-definition lands on the handler), and
#' `closure_func_range` carries the full handler extent so `map_occurrences`
#' computes the enclosing range via its closure fast path (no parse_data walk).
#'
#' @param ast_module Parsed AST module (from parse_file).
#' @param relative_path Document relative path for symbol IDs.
#' @param namespace Optional namespace prefix for symbol IDs.
#' @return List with `symbols` and `occurrences` element lists.
#' @keywords internal
build_plumber_symbol_entries <- function(ast_module, relative_path, namespace = NULL) {
  infos <- extract_plumber_symbols(ast_module)
  if (length(infos) == 0) {
    return(list(symbols = list(), occurrences = list()))
  }

  data <- ast_module$parse_data
  symbols <- list()
  occurrences <- list()
  seen <- character()

  # Resolve full-handler ranges (parent expr of each FUNCTION token) in one
  # vectorized match() rather than a per-handler scan.
  parent_ids <- vapply(infos, function(x) x$func_row$parent[1], integer(1))
  parent_idx <- match(parent_ids, data$id)

  for (k in seq_along(infos)) {
    info <- infos[[k]]
    symbol_id <- build_plumber_symbol_id(info$name, relative_path, namespace)

    # Dedupe within a file: two routes synthesizing the same id would produce
    # conflicting definitions; keep the first.
    if (symbol_id %in% seen) {
      next
    }
    seen <- c(seen, symbol_id)

    func_row <- info$func_row

    symbols[[length(symbols) + 1]] <- list(
      symbol = symbol_id,
      display_name = info$name,
      kind = "plumber_endpoint",
      documentation = info$doc
    )

    # Definition range = `function` keyword token (1-based parse_data -> 0-based
    # SCIP: subtract 1 from start line/col and end line, keep end col).
    def_range <- c(
      func_row$line1 - 1, func_row$col1 - 1,
      func_row$line2 - 1, func_row$col2
    )

    pidx <- parent_idx[k]
    func_range <- if (!is.na(pidx)) {
      parent_row <- data[pidx, ]
      c(
        parent_row$line1 - 1, parent_row$col1 - 1,
        parent_row$line2 - 1, parent_row$col2
      )
    } else {
      def_range
    }

    occurrences[[length(occurrences) + 1]] <- list(
      symbol = symbol_id,
      range = def_range,
      role = "definition",
      closure_func_range = func_range
    )
  }

  list(symbols = symbols, occurrences = occurrences)
}

#' Build a SCIP symbol identifier for plumber endpoints.
#'
#' Mirrors `build_shiny_symbol_id()`: namespaced format for package-exported
#' symbols, otherwise the canonical local-symbol format so semquery can locate
#' the endpoint by file path.
#'
#' @param name Symbol name.
#' @param relative_path Document path.
#' @param namespace Optional namespace.
#' @return Character symbol ID.
#' @keywords internal
build_plumber_symbol_id <- function(name, relative_path, namespace = NULL) {
  if (!is.null(namespace) && nzchar(namespace)) {
    esc_name <- escape_scip_identifier(name)
    sanitized_path <- gsub("[^A-Za-z0-9_/]", "_", relative_path)
    sprintf("%s %s#%s", namespace, sanitized_path, esc_name)
  } else {
    build_local_symbol_id(name, relative_path)
  }
}
