NULL

# Benchmarking utilities for performance analysis.
# Provides simple, togglable instrumentation for timing operations and counting calls.
# Output is JSON Lines format for easy parsing and analysis.

# Internal state for benchmark tracking
.bench_state <- new.env(parent = emptyenv())

#' Initialize benchmarking state.
#'
#' Sets up internal tracking structures. Called automatically on first use.
#'
#' @keywords internal
bench_init <- function() {
  bench_val <- Sys.getenv("SCIPR_BENCH", "0")
  .bench_state$enabled <- (bench_val == "1" || bench_val == "TRUE" || bench_val == "true")
  .bench_state$timers <- list()
  .bench_state$counts <- list()
  .bench_state$session_start <- Sys.time()
}

#' Check if benchmarking is enabled.
#'
#' @return Logical indicating if SCIPR_BENCH environment variable is set to 1.
#' @keywords internal
bench_enabled <- function() {
  if (!exists("enabled", envir = .bench_state)) {
    bench_init()
  }
  .bench_state$enabled
}

#' Start timing an operation.
#'
#' Records the start time for a named operation. If benchmarking is disabled,
#' this is a no-op with minimal overhead.
#'
#' @param op_name Character string identifying the operation.
#' @param metadata Optional named list of additional context (file, count, etc.).
#' @return Character token to pass to bench_end(), or NULL if benchmarking disabled.
#' @keywords internal
#' @examples
#' \dontrun{
#' Sys.setenv(SCIPR_BENCH = "1")
#' token <- bench_start("parse_file", list(file = "example.R"))
#' # ... do work ...
#' bench_end(token)
#' }
bench_start <- function(op_name, metadata = list()) {
  if (!bench_enabled()) {
    return(NULL)
  }

  # Generate unique token for this timing
  token <- paste0(op_name, "_", as.numeric(Sys.time()), "_", sample.int(1e6, 1))

  .bench_state$timers[[token]] <- list(
    op = op_name,
    start_time = Sys.time(),
    metadata = metadata
  )

  token
}

#' End timing an operation and output benchmark data.
#'
#' Calculates duration since bench_start() and prints JSON line to stdout.
#' If benchmarking is disabled or token is NULL, this is a no-op.
#'
#' @param token Character token returned by bench_start().
#' @param metadata Optional named list to merge with start metadata.
#' @keywords internal
#' @examples
#' \dontrun{
#' token <- bench_start("resolve_symbols")
#' # ... do work ...
#' bench_end(token, list(symbol_count = 42))
#' }
bench_end <- function(token, metadata = list()) {
  if (!bench_enabled() || is.null(token)) {
    return(invisible(NULL))
  }

  if (!exists("timers", envir = .bench_state) || !token %in% names(.bench_state$timers)) {
    return(invisible(NULL))
  }

  timer <- .bench_state$timers[[token]]
  end_time <- Sys.time()
  duration_ms <- as.numeric(difftime(end_time, timer$start_time, units = "secs")) * 1000

  # Merge metadata
  all_metadata <- c(timer$metadata, metadata)

  # Build JSON output
  json_parts <- c(
    paste0('"op":"', timer$op, '"'),
    paste0('"duration_ms":', format(duration_ms, digits = 4)),
    paste0('"timestamp":"', format(end_time, "%Y-%m-%dT%H:%M:%OS3"), '"')
  )

  # Add metadata fields
  if (length(all_metadata) > 0) {
    for (name in names(all_metadata)) {
      val <- all_metadata[[name]]
      if (is.character(val)) {
        json_parts <- c(json_parts, paste0('"', name, '":"', val, '"'))
      } else if (is.numeric(val)) {
        json_parts <- c(json_parts, paste0('"', name, '":', val))
      } else if (is.logical(val)) {
        json_parts <- c(json_parts, paste0('"', name, '":', tolower(as.character(val))))
      }
    }
  }

  cat("[BENCH] {", paste(json_parts, collapse = ","), "}\n", sep = "")

  # Clean up timer
  .bench_state$timers[[token]] <- NULL

  invisible(NULL)
}

#' Increment a call counter.
#'
#' Tracks how many times an operation is called. Useful for detecting
#' unexpectedly high call counts in loops.
#'
#' @param op_name Character string identifying the operation.
#' @keywords internal
#' @examples
#' \dontrun{
#' for (i in 1:100) {
#'   bench_count("tree_walk")
#'   # ... do work ...
#' }
#' bench_report_counts()
#' }
bench_count <- function(op_name) {
  if (!bench_enabled()) {
    return(invisible(NULL))
  }

  if (!exists("counts", envir = .bench_state)) {
    .bench_state$counts <- list()
  }

  if (op_name %in% names(.bench_state$counts)) {
    .bench_state$counts[[op_name]] <- .bench_state$counts[[op_name]] + 1
  } else {
    .bench_state$counts[[op_name]] <- 1L
  }

  invisible(NULL)
}

#' Report accumulated call counts.
#'
#' Outputs a JSON line for each counted operation with total call count.
#' Typically called at end of a phase or entire run.
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' bench_report_counts()
#' }
bench_report_counts <- function() {
  if (!bench_enabled() || !exists("counts", envir = .bench_state)) {
    return(invisible(NULL))
  }

  for (op_name in names(.bench_state$counts)) {
    count <- .bench_state$counts[[op_name]]
    cat(
      '[BENCH] {"op":"', op_name, '",',
      '"call_count":', count, ",",
      '"timestamp":"', format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"), '"}',
      "\n",
      sep = ""
    )
  }

  invisible(NULL)
}

#' Wrapper to benchmark a code block.
#'
#' Convenience function to time an arbitrary expression.
#'
#' @param op_name Character string identifying the operation.
#' @param expr Expression to evaluate and time.
#' @param metadata Optional named list of context metadata.
#' @return The result of evaluating expr.
#' @keywords internal
#' @examples
#' \dontrun{
#' result <- bench_expr(
#'   "expensive_operation",
#'   {
#'     Sys.sleep(0.1)
#'     42
#'   },
#'   list(input_size = 100)
#' )
#' }
bench_expr <- function(op_name, expr, metadata = list()) {
  token <- bench_start(op_name, metadata)
  result <- force(expr)
  bench_end(token)
  result
}
