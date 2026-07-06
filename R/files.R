#' @include utils.R
#' @include workspace.R
NULL

# Regex matching R source files (.R or .r). Single source of truth shared by the
# directory-pruning walk and the NULL-include fallback so the two never diverge.
R_SOURCE_PATTERN <- "\\.[Rr]$"

# =============================================================================
# File Discovery Module
# =============================================================================
#
# Enumerates R source files for SCIP indexing with support for:
#   - Gitignore/Rbuildignore exclusion patterns
#   - Include patterns for selective indexing (like scip-go's directory args)
#   - Incremental indexing via file hash caching
#
# Design notes:
#   - Relative paths are computed BEFORE filtering to handle renv symlinks
#     that resolve outside the project root (e.g., to ~/.cache/R/renv/...)
#   - Gitignore semantics use three matching scopes: component (filename),
#     suffix (trailing path segments), and path (full relative path)
#   - Directory patterns (trailing /) match any file beneath that directory
#   - Include patterns act as an allowlist when specified
#
# =============================================================================

#' Discover source files for indexing.
#'
#' Enumerates all `.R` files under the project root, applies exclusion rules
#' from `.gitignore` and `.Rbuildignore`, optionally filters by include
#' patterns, and maintains a cache manifest for incremental indexing.
#'
#' @param workspace Workspace list produced by `analyze_workspace()`.
#' @param cache_enabled Logical; when TRUE, loads/saves a manifest to skip
#'   unchanged files on subsequent runs.
#' @param include_patterns Character vector of glob patterns for selective
#'   indexing. When provided, only files matching at least one pattern are
#'   indexed. Patterns match against relative paths (e.g., "R/*.R",
#'   "components/**/*.R"). When NULL (default), all non-ignored files are
#'   indexed.
#' @return A list with `files` (list of file descriptors) and `cache` (status).
#' @keywords internal
discover_files <- function(workspace, cache_enabled = FALSE, include_patterns = NULL) {
  root <- workspace$metadata$project_root
  ignore_rules <- workspace$ignore %||% list()

  # Cache management

  state_path <- file.path(root, ".scip-r", "state.rds")
  previous_state <- if (cache_enabled) load_cache_state(state_path) else list()
  current_state <- list()

  # Enumerate files using relative paths to avoid symlink resolution issues.
  # renv creates symlinks in renv/library/ pointing to a global cache; using
  # full.names=TRUE would resolve these, causing gitignore patterns to fail.
  #
  # When include patterns are present we use a directory-pruning walk
  # (enumerate_r_files) that skips whole subtrees that cannot contain a kept
  # file (e.g. renv/ when scoped to "R/**/*.R") BEFORE traversing them. This
  # avoids list.files(recursive=TRUE) descending into renv's library symlink
  # (thousands of cached-package .R files). Pruning only removes subtrees that
  # the post-walk filter loop would discard anyway, so the candidate set — and
  # therefore the indexed document set — is identical to the unpruned walk.
  if (!is.null(include_patterns) && length(include_patterns) > 0) {
    parsed_includes <- parse_include_segments(include_patterns)
    has_negation <- any(vapply(
      ignore_rules,
      function(r) is.list(r) && isTRUE(r$negate),
      logical(1)
    ))
    all_relative <- enumerate_r_files(
      root, ignore_rules, parsed_includes, has_negation
    )
  } else {
    all_relative <- sort(list.files(
      root,
      pattern = R_SOURCE_PATTERN,
      recursive = TRUE,
      full.names = FALSE
    ))
  }

  if (length(all_relative) == 0) {
    return(list(
      files = list(),
      cache = cache_summary(cache_enabled, state_path, previous_state, current_state)
    ))
  }

  # Pre-compile include patterns to regex for efficiency
  include_regexes <- compile_include_patterns(include_patterns)

  # Filter and collect file metadata
  files <- list()

  for (relative in all_relative) {
    # Step 1: Apply exclusion rules (gitignore, Rbuildignore)
    if (matches_ignore(relative, ignore_rules)) {
      next
    }

    # Step 2: Apply include filter (allowlist) if specified
    if (!is.null(include_regexes) && !matches_any_pattern(relative, include_regexes)) {
      next
    }

    # Step 3: Collect file metadata
    path <- file.path(root, relative)
    hash <- digest_file(path)
    info <- file.info(path)

    needs_index <- TRUE
    if (cache_enabled && length(previous_state) > 0) {
      prev <- previous_state[[relative]]
      needs_index <- is.null(prev) || !identical(prev$hash, hash)
    }

    files[[length(files) + 1]] <- list(
      path = path,
      relative_path = relative,
      hash = hash,
      size = info$size,
      mtime = unname(info$mtime),
      needs_index = needs_index
    )

    current_state[[relative]] <- list(
      hash = hash,
      size = info$size,
      mtime = unname(info$mtime)
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

# =============================================================================
# Include Pattern Matching
# =============================================================================

#' Compile include patterns to regex.
#'
#' @param patterns Character vector of glob patterns, or NULL.
#' @return Character vector of regex patterns, or NULL if no patterns.
#' @keywords internal
compile_include_patterns <- function(patterns) {
  if (is.null(patterns) || length(patterns) == 0) {
    return(NULL)
  }
  vapply(patterns, glob_to_regex, character(1), USE.NAMES = FALSE)
}

#' Test if a path matches any include pattern.
#'
#' @param path Relative file path.
#' @param regexes Character vector of compiled regex patterns.
#' @return TRUE if path matches at least one pattern.
#' @keywords internal
matches_any_pattern <- function(path, regexes) {
  for (rx in regexes) {
    if (grepl(rx, path, perl = TRUE)) {
      return(TRUE)
    }
  }
  FALSE
}

# =============================================================================
# Directory-Pruning Walk
# =============================================================================

#' Pre-split include globs into per-segment matchers.
#'
#' Each pattern becomes a list of segments (split on "/"). A "**" segment is
#' tagged with `star2 = TRUE` (matches zero or more directory segments); every
#' other segment carries a compiled single-segment regex (via `glob_to_regex`,
#' the same machine the post-walk filter uses, so the two never disagree).
#'
#' @param patterns Character vector of include glob patterns.
#' @return List of per-pattern segment lists.
#' @keywords internal
parse_include_segments <- function(patterns) {
  lapply(patterns, function(p) {
    segs <- strsplit(p, "/", fixed = TRUE)[[1]]
    lapply(segs, function(s) {
      if (identical(s, "**")) {
        list(star2 = TRUE)
      } else {
        list(star2 = FALSE, regex = glob_to_regex(s))
      }
    })
  })
}

#' Can a directory subtree contain a file matching this segment pattern?
#'
#' Greedily matches directory segments against pattern segments, treating a
#' "**" segment as zero-or-more directory segments. Returns TRUE when, after
#' consuming all directory segments, at least one pattern segment remains to
#' match a file below (so the directory is worth descending into). Conservative
#' by construction: it returns TRUE whenever *some* descendant could match, so a
#' directory holding a kept file is never pruned.
#'
#' @param psegs Pattern segment list from `parse_include_segments`.
#' @param pi Current pattern-segment index.
#' @param dsegs Character vector of directory path segments.
#' @param di Current directory-segment index.
#' @return Logical.
#' @keywords internal
seg_descend <- function(psegs, pi, dsegs, di) {
  np <- length(psegs)
  nd <- length(dsegs)

  # Consumed all directory segments: a file can live below iff the pattern
  # still has at least one segment left to match it.
  if (di > nd) {
    return(pi <= np)
  }
  # Pattern exhausted but directory continues: nothing below can match.
  if (pi > np) {
    return(FALSE)
  }

  p <- psegs[[pi]]
  if (isTRUE(p$star2)) {
    # "**" matches zero directory segments (advance pattern) ...
    if (seg_descend(psegs, pi + 1L, dsegs, di)) {
      return(TRUE)
    }
    # ... or one-or-more (consume a directory segment, "**" stays put).
    return(seg_descend(psegs, pi, dsegs, di + 1L))
  }

  if (grepl(p$regex, dsegs[[di]], perl = TRUE)) {
    return(seg_descend(psegs, pi + 1L, dsegs, di + 1L))
  }
  FALSE
}

#' Whether a directory could contain a file matching any include pattern.
#'
#' @param dir_rel Relative directory path (forward slashes, no trailing slash).
#' @param parsed Parsed include segments from `parse_include_segments`.
#' @return TRUE if the subtree could contain a matching file.
#' @keywords internal
dir_could_contain_include <- function(dir_rel, parsed) {
  dsegs <- strsplit(dir_rel, "/", fixed = TRUE)[[1]]
  for (psegs in parsed) {
    if (seg_descend(psegs, 1L, dsegs, 1L)) {
      return(TRUE)
    }
  }
  FALSE
}

#' Whether a directory-only ignore rule covers an entire subtree.
#'
#' A `dir_only` gitignore rule (trailing slash, e.g. `renv/`) ignores the whole
#' directory and everything beneath it. Matching such a rule against the
#' directory itself therefore lets us prune the subtree. The caller must ensure
#' no negation rules exist — a `!` elsewhere could re-include a descendant —
#' which is what keeps this safe.
#'
#' @param dir_rel Relative directory path.
#' @param rules Ignore rules from `detect_ignore_rules`.
#' @return TRUE if a dir-only rule ignores this directory.
#' @keywords internal
dir_only_ignored <- function(dir_rel, rules) {
  if (length(rules) == 0) {
    return(FALSE)
  }
  components <- strsplit(dir_rel, "/", fixed = TRUE)[[1]]
  suffixes <- NULL
  dir_prefixes <- NULL

  for (rule in rules) {
    if (is.null(rule) || is.character(rule)) next
    if (!isTRUE(rule$dir_only) || isTRUE(rule$negate)) next

    # Candidate derivation deliberately parallels matches_ignore() but in
    # DIRECTORY mode, not file mode: here the path's last component IS a
    # directory, so `component` keeps every segment (matches_ignore drops the
    # trailing filename for dir_only file paths). Kept separate on purpose — a
    # shared helper would need a file-vs-dir flag that obscures both call sites.
    scope <- rule$scope %||% "path"
    candidates <- switch(scope,
      component = components,
      suffix = {
        if (is.null(suffixes)) suffixes <- build_path_suffixes(components)
        suffixes
      },
      path = {
        if (is.null(dir_prefixes)) dir_prefixes <- build_dir_prefixes(components)
        c(dir_rel, dir_prefixes)
      },
      dir_rel
    )

    for (cand in candidates) {
      if (nzchar(cand) && grepl(rule$regex, cand, perl = TRUE)) {
        return(TRUE)
      }
    }
  }
  FALSE
}

#' Enumerate `.R` files via a directory-pruning recursive walk.
#'
#' Like `list.files(pattern = R_SOURCE_PATTERN, recursive = TRUE)` (with
#' `all.files = FALSE`) but skips whole subtrees that cannot contain a kept
#' file: a subdirectory is not descended into when (a) it cannot contain any
#' include match, or (b) a directory-only ignore rule covers it and no
#' negation rules are present. Hidden entries (leading ".") are skipped,
#' matching `all.files = FALSE`.
#'
#' @param root Project root (absolute).
#' @param ignore_rules Ignore rules from `detect_ignore_rules`.
#' @param parsed_includes Parsed include segments, or NULL.
#' @param has_negation TRUE if any ignore rule negates (disables ignore pruning).
#' @return Sorted character vector of relative `.R` file paths.
#' @keywords internal
enumerate_r_files <- function(root, ignore_rules, parsed_includes, has_negation) {
  out <- character()

  walk <- function(dir_rel) {
    abs_dir <- if (nzchar(dir_rel)) file.path(root, dir_rel) else root

    entries <- list.files(abs_dir, all.files = FALSE, full.names = FALSE, no.. = TRUE)
    if (length(entries) == 0) {
      return(invisible())
    }

    subdirs <- list.dirs(abs_dir, recursive = FALSE, full.names = FALSE)
    subdirs <- subdirs[!startsWith(subdirs, ".")]

    files <- setdiff(entries, subdirs)
    # R source files use either .R or .r; match both (the original list.files
    # pattern was case-sensitive .R only). Downstream include patterns still
    # govern which extensions are actually indexed.
    r_files <- files[grepl(R_SOURCE_PATTERN, files)]
    if (length(r_files) > 0) {
      rels <- if (nzchar(dir_rel)) paste0(dir_rel, "/", r_files) else r_files
      out <<- c(out, rels)
    }

    for (sd in subdirs) {
      sd_rel <- if (nzchar(dir_rel)) paste0(dir_rel, "/", sd) else sd

      if (!is.null(parsed_includes) &&
        !dir_could_contain_include(sd_rel, parsed_includes)) {
        next
      }
      if (!has_negation && dir_only_ignored(sd_rel, ignore_rules)) {
        next
      }
      walk(sd_rel)
    }
    invisible()
  }

  walk("")
  sort(out)
}

# =============================================================================
# Ignore Pattern Matching
# =============================================================================

#' Test whether a path matches any ignore rule.
#'
#' Evaluates rules in order, tracking ignored/negated state. Supports three
#' matching scopes: component (individual path segments), suffix (trailing
#' path portions), and path (full relative path). Directory-only patterns
#' match files anywhere beneath the specified directory.
#'
#' @param path Relative file path using forward slashes.
#' @param rules List of ignore rules from `detect_ignore_rules()`.
#' @return TRUE if the path should be ignored, FALSE otherwise.
#' @keywords internal
matches_ignore <- function(path, rules) {
  if (length(rules) == 0) {
    return(FALSE)
  }

  # Legacy support for simple regex character vectors
  if (is.character(rules)) {
    return(any(vapply(rules, function(pat) {
      nzchar(pat) && grepl(pat, path, perl = TRUE)
    }, logical(1))))
  }

  components <- strsplit(path, "/", fixed = TRUE)[[1]]
  if (length(components) == 0) {
    components <- path
  }

  # Lazy-compute path variants only when needed
  suffixes <- NULL
  dir_prefixes <- NULL
  ignored <- FALSE

  for (rule in rules) {
    if (is.null(rule)) next

    # Legacy: bare regex string
    if (is.character(rule)) {
      if (grepl(rule, path, perl = TRUE)) {
        ignored <- TRUE
      }
      next
    }

    scope <- rule$scope %||% "path"
    dir_only <- isTRUE(rule$dir_only)

    # Select match candidates based on scope
    candidates <- switch(scope,
      component = {
        if (dir_only) {
          # Exclude filename for directory patterns
          if (length(components) > 1) components[-length(components)] else character()
        } else {
          components
        }
      },
      suffix = {
        if (is.null(suffixes)) {
          suffixes <- build_path_suffixes(components)
        }
        if (dir_only) {
          # For directory patterns, also match directory prefixes
          # e.g., "renv/library/" should match "renv/library/pkg/R/file.R"
          if (is.null(dir_prefixes)) {
            dir_prefixes <- build_dir_prefixes(components)
          }
          unique(c(suffixes[grepl("/", suffixes, fixed = TRUE)], dir_prefixes))
        } else {
          suffixes
        }
      },
      path = {
        if (dir_only && !grepl("/", path, fixed = TRUE)) {
          character()
        } else {
          path
        }
      },
      path
    )

    if (length(candidates) == 0) next

    # Check if any candidate matches the rule's regex
    matched <- FALSE
    for (cand in candidates) {
      if (nzchar(cand) && grepl(rule$regex, cand, perl = TRUE)) {
        matched <- TRUE
        break
      }
    }

    if (matched) {
      ignored <- !isTRUE(rule$negate)
    }
  }

  ignored
}

# =============================================================================
# Path Variant Builders
# =============================================================================

#' Build suffix variants of a path (trailing segments).
#'
#' For path "a/b/c/file.R", returns:
#'   c("a/b/c/file.R", "b/c/file.R", "c/file.R", "file.R")
#'
#' @param components Character vector of path segments.
#' @return Character vector of path suffixes.
#' @keywords internal
build_path_suffixes <- function(components) {
  n <- length(components)
  if (n == 0) {
    return(character())
  }

  vapply(seq_len(n), function(i) {
    paste(components[i:n], collapse = "/")
  }, character(1))
}

#' Build directory prefix variants of a path.
#'
#' For path "a/b/c/file.R", returns: c("a", "a/b", "a/b/c")
#' Used for matching directory-only patterns like "renv/library/".
#'
#' @param components Character vector of path segments.
#' @return Character vector of directory prefixes (excludes filename).
#' @keywords internal
build_dir_prefixes <- function(components) {
  n <- length(components)
  if (n <= 1) {
    return(character())
  }

  dir_parts <- components[-n]
  vapply(seq_along(dir_parts), function(i) {
    paste(dir_parts[1:i], collapse = "/")
  }, character(1))
}

# =============================================================================
# Path Utilities
# =============================================================================

#' Compute relative path from root.
#'
#' @param path Absolute file path.
#' @param root Workspace root directory.
#' @return Relative path with forward slashes.
#' @keywords internal
rel_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")

  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    gsub("\\\\", "/", path)
  }
}

#' Compute MD5 hash of a file.
#'
#' Falls back to size+mtime fingerprint if digest package unavailable.
#'
#' @param path File path.
#' @return Hash string or NA if file doesn't exist.
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

# =============================================================================
# Cache Management
# =============================================================================

#' Load cached file state from disk.
#'
#' @param state_path Path to the RDS manifest file.
#' @return Named list of file metadata, or empty list if unavailable.
#' @keywords internal
load_cache_state <- function(state_path) {
  if (!file.exists(state_path)) {
    return(list())
  }

  tryCatch(
    {
      state <- readRDS(state_path)
      if (is.list(state)) state else list()
    },
    error = function(e) list()
  )
}

#' Save file state cache to disk.
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
#' @param enabled Whether caching is enabled.
#' @param state_path Manifest file location.
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
