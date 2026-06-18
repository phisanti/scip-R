NULL

# =============================================================================
# Workspace Analysis Module
# =============================================================================
#
# This module inspects R project workspaces to extract metadata, dependencies,
# and file exclusion rules for SCIP indexing. It serves as the foundation for
# understanding project structure before file discovery begins.
#
# Supported project types:
#   - Standard R packages (DESCRIPTION file)
#   - renv-managed projects (renv.lock)
#   - packrat-managed projects (packrat.lock)
#   - Generic R projects (directory of .R files)
#
# =============================================================================

#' Analyze workspace metadata and dependencies.
#'
#' Inspects the project root to determine project identity, version, external
#' dependencies, and file exclusion rules from `.gitignore` and `.Rbuildignore`.
#'
#' @param root Character path to the project root directory.
#' @param namespace Optional symbol namespace prefix for emitted SCIP symbols.
#' @return A list containing:
#'   - `metadata`: project_root, project_uri, project_name, project_version, namespace
#'   - `dependencies`: named character vector of package -> version mappings
#'   - `ignore`: list of ignore rules for file discovery
#' @keywords internal
analyze_workspace <- function(root, namespace = NULL) {
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)

  metadata <- list(
    project_root = root,
    project_uri = paste0("file://", root),
    project_name = basename(root),
    project_version = detect_project_version(root),
    namespace = namespace
  )

  list(
    metadata = metadata,
    dependencies = detect_dependencies(root),
    ignore = detect_ignore_rules(root)
  )
}

# =============================================================================
# Project Metadata Detection
# =============================================================================

#' Detect project version from DESCRIPTION.
#'
#' @param root Project root path.
#' @return Version string, or "0.0.0-dev" if not found.
#' @keywords internal
detect_project_version <- function(root) {
  desc_path <- file.path(root, "DESCRIPTION")

  if (!file.exists(desc_path)) {
    return("0.0.0-dev")
  }

  lines <- readLines(desc_path, warn = FALSE)
  version_line <- lines[grepl("^Version:", lines)]

  if (length(version_line) == 0) {
    return("0.0.0-dev")
  }

  trimws(sub("^Version:[[:space:]]*", "", version_line[[1]]))
}

#' Detect external dependencies from DESCRIPTION and lockfiles.
#'
#' Parses DESCRIPTION for Imports/Depends/Suggests, and renv.lock or
#' packrat.lock for pinned versions.
#'
#' @param root Project root path.
#' @return Named character vector: package names -> versions (NA if unknown).
#' @keywords internal
detect_dependencies <- function(root) {
  deps <- character()

  # Parse DESCRIPTION
  desc_path <- file.path(root, "DESCRIPTION")
  if (file.exists(desc_path)) {
    lines <- readLines(desc_path, warn = FALSE)
    dep_lines <- lines[grepl("^(Imports|Depends|Suggests):", lines)]

    if (length(dep_lines) > 0) {
      combined <- paste(dep_lines, collapse = " ")
      pkgs <- unlist(strsplit(combined, "[,()[:space:]]"))
      pkgs <- trimws(pkgs)
      pkgs <- pkgs[nzchar(pkgs) & !grepl("^(Imports|Depends|Suggests)", pkgs)]
      deps[pkgs] <- NA_character_
    }
  }

  # Parse lockfiles for version information
  for (lockfile in c("renv.lock", "packrat.lock")) {
    lock_path <- file.path(root, lockfile)
    if (!file.exists(lock_path)) next

    lock_data <- readLines(lock_path, warn = FALSE)
    pkg_lines <- lock_data[grepl("\"Package\"", lock_data)]
    ver_lines <- lock_data[grepl("\"Version\"", lock_data)]

    if (length(pkg_lines) == length(ver_lines)) {
      pkgs <- trimws(sub('.*"Package"[[:space:]]*:[[:space:]]*"(.*?)".*', "\\1", pkg_lines))
      vers <- trimws(sub('.*"Version"[[:space:]]*:[[:space:]]*"(.*?)".*', "\\1", ver_lines))
      deps[pkgs] <- vers
    }
  }

  deps
}

# =============================================================================
# Ignore Rule Detection
# =============================================================================

#' Detect file exclusion rules from project configuration.
#'
#' Combines built-in exclusions, .Rbuildignore patterns (as regex), and
#' .gitignore patterns (parsed as gitignore syntax) into a unified rule set.
#'
#' @param root Project root path.
#' @return List of ignore rules for use with `matches_ignore()`.
#' @keywords internal
detect_ignore_rules <- function(root) {
  rules <- list()

  # Built-in exclusions: directories that should never be indexed
  builtin_patterns <- c(".git/", ".Rproj.user/", ".renv/")
  rules <- c(rules, parse_gitignore_lines(builtin_patterns))

  # .Rbuildignore: regex patterns matched against full path
  build_ignore <- file.path(root, ".Rbuildignore")
  if (file.exists(build_ignore)) {
    patterns <- read_ignore_file(build_ignore, strip_comments = TRUE)
    for (pattern in patterns) {
      rules[[length(rules) + 1]] <- list(
        regex = pattern,
        negate = FALSE,
        scope = "path",
        raw = paste0(".Rbuildignore:", pattern)
      )
    }
  }

  # .gitignore: parsed using gitignore semantics
  git_ignore <- file.path(root, ".gitignore")
  if (file.exists(git_ignore)) {
    patterns <- read_ignore_file(git_ignore, strip_comments = FALSE)
    rules <- c(rules, parse_gitignore_lines(patterns))
  }

  rules
}

# =============================================================================
# Gitignore Parsing
# =============================================================================

#' Read and clean lines from an ignore file.
#'
#' @param path File path to read.
#' @param strip_comments If TRUE, remove lines starting with #.
#' @return Character vector of non-empty lines.
#' @keywords internal
read_ignore_file <- function(path, strip_comments = TRUE) {
  lines <- readLines(path, warn = FALSE)
  if (length(lines) == 0) return(character())

  cleaned <- character()
  for (line in lines) {
    line <- sub("\r$", "", line, perl = TRUE)
    line <- trimws(line, which = "right")

    if (strip_comments && startsWith(line, "#")) next
    if (!nzchar(line)) next

    cleaned <- c(cleaned, line)
  }
  cleaned
}

#' Parse gitignore lines into structured rules.
#'
#' @param lines Character vector of gitignore entries.
#' @return List of rule objects compatible with `matches_ignore()`.
#' @keywords internal
parse_gitignore_lines <- function(lines) {
  if (length(lines) == 0) return(list())

  rules <- list()
  for (line in lines) {
    rule <- parse_gitignore_line(line)
    if (!is.null(rule)) {
      rules[[length(rules) + 1]] <- rule
    }
  }
  rules
}

#' Parse a single gitignore line into a rule.
#'
#' Implements a subset of gitignore semantics:
#'   - Comments (#) and blank lines are ignored
#'   - Negation prefix (!) un-ignores previously matched paths
#'   - Leading slash (/) anchors pattern to project root
#'   - Trailing slash (/) matches only directories
#'   - Glob wildcards (*, ?, []) are converted to regex
#'
#' @param line Single line from a gitignore file.
#' @return Rule list with regex, negate, scope, dir_only, raw; or NULL.
#' @keywords internal
parse_gitignore_line <- function(line) {
  if (is.null(line)) return(NULL)

  original <- line
  line <- sub("\r$", "", line, perl = TRUE)
  line <- trimws(line, which = "right")

  if (!nzchar(line)) return(NULL)

  # Handle comments (escaped or not)
  if (startsWith(line, "\\#")) {
    line <- substring(line, 2)
  } else if (startsWith(line, "#")) {
    return(NULL)
  }

  # Handle negation
  if (startsWith(line, "\\!")) {
    line <- substring(line, 2)
  }

  negate <- FALSE
  if (startsWith(line, "!")) {
    negate <- TRUE
    line <- substring(line, 2)
  }

  # Unescape special characters
  line <- gsub("\\\\ ", " ", line, perl = TRUE)
  line <- gsub("\\\\#", "#", line, perl = TRUE)
  line <- gsub("\\\\!", "!", line, perl = TRUE)

  if (!nzchar(line)) return(NULL)

  # Determine matching behavior
  anchored <- startsWith(line, "/")
  pattern <- sub("^/", "", line)
  dir_only <- endsWith(pattern, "/")
  pattern <- sub("/+$", "", pattern)
  has_slash <- grepl("/", pattern, fixed = TRUE)

  regex <- glob_to_regex(pattern)
  if (!nzchar(regex)) return(NULL)

  # Scope determines how pattern is matched:
  #   - "path": match against full relative path (anchored patterns)
  #   - "suffix": match against trailing path segments (patterns with /)
  #   - "component": match against individual path segments (simple patterns)
  scope <- if (anchored) {
    "path"
  } else if (has_slash) {
    "suffix"
  } else {
    "component"
  }

  list(
    regex = regex,
    negate = negate,
    scope = scope,
    dir_only = dir_only,
    raw = original
  )
}

#' Convert a shell glob pattern to a regular expression.
#'
#' @param pattern Glob pattern (e.g., "*.R", "test_*").
#' @return Anchored regular expression string.
#' @keywords internal
glob_to_regex <- function(pattern) {
  if (!nzchar(pattern)) {
    return("^.*$")
  }

  chars <- strsplit(pattern, "", fixed = TRUE)[[1]]
  out <- "^"
  i <- 1L
  n <- length(chars)
  regex_meta <- c("\\", ".", "(", ")", "[", "]", "{", "}", "+", "^", "$", "|")

  while (i <= n) {
    ch <- chars[i]

    if (ch == "*") {
      # "**/" should match zero or more directory segments, so patterns like
      # "R/**/*.R" also match top-level files such as "R/foo.R".
      if ((i + 1L) <= n && chars[i + 1L] == "*") {
        i <- i + 2L
        if (i <= n && chars[i] == "/") {
          out <- paste0(out, "(?:[^/]+/)*")
          i <- i + 1L
        } else {
          out <- paste0(out, ".*")
        }
        next
      }

      out <- paste0(out, "[^/]*")
      i <- i + 1L
      next
    }

    if (ch == "?") {
      out <- paste0(out, "[^/]")
      i <- i + 1L
      next
    }

    if (ch %in% regex_meta) {
      out <- paste0(out, "\\", ch)
    } else {
      out <- paste0(out, ch)
    }

    i <- i + 1L
  }

  paste0(out, "$")
}

# Legacy alias for backwards compatibility
detect_ignore_globs <- detect_ignore_rules
