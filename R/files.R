#' @include utils.R
#' @include workspace.R
NULL

# This script manages file discovery and change planning for scip-r runs.
# It enumerates source files, applies ignore rules, and prepares indexing queues.
# Future iterations should enhance caching and incremental decision logic.

#' Discover source files for indexing.
#'
#' Applies workspace ignore rules, computes file hashes, and optionally tracks
#' a cache manifest to skip unchanged files on subsequent runs.
#'
#' @param workspace Workspace list produced by `analyze_workspace()`.
#' @param cache_enabled Logical flag enabling cache hints and manifest writes.
#' @return A list containing discovered files and cache metadata.
#' @keywords internal
#' @examples
#' \dontrun{
#' discover_files(list(metadata = list(project_root = tempdir()), ignore = character()))
#' }
discover_files <- function(workspace, cache_enabled = FALSE) {
  root <- workspace$metadata$project_root
  ignore <- workspace$ignore %||% list()
  state_path <- file.path(root, ".scip-r", "state.rds")
  previous_state <- if (cache_enabled) load_cache_state(state_path) else list()
  current_state <- list()

  all_files <- list.files(root, pattern = "\\.R$", recursive = TRUE, full.names = TRUE)
  if (length(all_files) == 0) {
    return(list(
      files = list(),
      cache = cache_summary(cache_enabled, state_path, previous_state, current_state)
    ))
  }

  all_files <- sort(all_files)

  files <- list()
  for (path in all_files) {
    relative <- rel_path(path, root)
    if (matches_ignore(relative, ignore)) {
      next
    }

    hash <- digest_file(path)
    file_info <- file.info(path)
    needs_index <- TRUE
    if (cache_enabled && length(previous_state) > 0) {
      prev <- previous_state[[relative]]
      needs_index <- is.null(prev) || !identical(prev$hash, hash)
    }

    files[[length(files) + 1]] <- list(
      path = path,
      relative_path = relative,
      hash = hash,
      size = file_info$size,
      mtime = unname(file_info$mtime),
      needs_index = needs_index
    )

    current_state[[relative]] <- list(
      hash = hash,
      size = file_info$size,
      mtime = unname(file_info$mtime)
    )
  }

  if (cache_enabled) {
    save_cache_state(state_path, current_state)
  }

  list(
    files = files,
    cache = cache_summary(cache_enabled, state_path, previous_state, current_state)
  )
}

#' Check if path matches ignore rules.
#'
#' @param path File path to evaluate.
#' @param ignore Character vector of ignore patterns.
#' @return Logical indicating whether the path should be ignored.
#' @keywords internal
matches_ignore <- function(path, ignore) {
  if (length(ignore) == 0) {
    return(FALSE)
  }

  # Backwards compatibility for character vectors
  if (is.character(ignore)) {
    return(any(vapply(ignore, function(pattern) {
      nzchar(pattern) && grepl(pattern, path, perl = TRUE)
    }, logical(1))))
  }

  components <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(components) == 0) {
    components <- path
  }
  suffixes <- NULL

  ignored <- FALSE
  for (rule in ignore) {
    if (is.null(rule)) {
      next
    }

    if (is.character(rule)) {
      if (grepl(rule, path, perl = TRUE)) {
        ignored <- TRUE
      }
      next
    }

    scope <- rule$scope %||% "path"
    candidates <- switch(
      scope,
      component = components,
      suffix = {
        if (is.null(suffixes)) {
          suffixes <- build_path_suffixes(components)
        }
        suffixes
      },
      path = path,
      path
    )

    if (isTRUE(rule$dir_only)) {
      if (identical(scope, "component")) {
        if (length(candidates) <= 1) {
          candidates <- character()
        } else {
          candidates <- candidates[-length(candidates)]
        }
      } else if (identical(scope, "suffix")) {
        candidates <- candidates[grepl("/", candidates, fixed = TRUE)]
      } else if (identical(scope, "path") && !grepl("/", path, fixed = TRUE)) {
        candidates <- character()
      }
    }

    if (length(candidates) == 0) {
      next
    }

    matched <- any(vapply(candidates, function(candidate) {
      nzchar(candidate) && grepl(rule$regex, candidate, perl = TRUE)
    }, logical(1)))

    if (matched) {
      if (isTRUE(rule$negate)) {
        ignored <- FALSE
      } else {
        ignored <- TRUE
      }
    }
  }

  ignored
}

#' Relative path helper.
#'
#' @param path Absolute file path.
#' @param root Workspace root.
#' @return Relative path with forward slashes.
#' @keywords internal
rel_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) {
    rel <- substring(path, nchar(prefix) + 1L)
  } else {
    rel <- path
  }
  gsub("\\\\", "/", rel)
}

#' Digest helper using MD5 hash when available.
#'
#' @param path File path.
#' @return MD5 hash string or NA if hashing fails.
#' @keywords internal
digest_file <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(file = path, algo = "md5"))
  }
  info <- file.info(path)
  paste(info$size, as.numeric(info$mtime), sep = "-")
}

#' Build suffix variants for a relative path.
#'
#' @param components Character vector of path components.
#' @return Character vector of suffixes joining trailing components.
#' @keywords internal
build_path_suffixes <- function(components) {
  n <- length(components)
  if (n == 0) {
    return(character())
  }
  suffixes <- character(n)
  for (i in seq_len(n)) {
    suffixes[i] <- paste(components[i:n], collapse = "/")
  }
  suffixes
}

#' Load cached manifest state.
#'
#' @param state_path RDS path storing file hashes.
#' @return Named list of cached entries.
#' @keywords internal
load_cache_state <- function(state_path) {
  if (!file.exists(state_path)) {
    return(list())
  }
  tryCatch({
    state <- readRDS(state_path)
    if (is.list(state)) state else list()
  }, error = function(e) {
    list()
  })
}

#' Persist cache manifest state.
#'
#' @param state_path Destination RDS path.
#' @param state Named list of file metadata.
#' @keywords internal
save_cache_state <- function(state_path, state) {
  dir_path <- dirname(state_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(state, state_path)
}

#' Summarize cache status for discover_files output.
#'
#' @param enabled Logical flag.
#' @param state_path Manifest location.
#' @param previous_state Previously loaded entries.
#' @param current_state Newly computed entries.
#' @return List describing cache state.
#' @keywords internal
cache_summary <- function(enabled, state_path, previous_state, current_state) {
  list(
    enabled = isTRUE(enabled),
    state_path = if (isTRUE(enabled)) state_path else NULL,
    previous_entries = if (is.list(previous_state)) length(previous_state) else 0L,
    current_entries = if (is.list(current_state)) length(current_state) else 0L,
    updated_at = if (isTRUE(enabled)) Sys.time() else NULL
  )
}
