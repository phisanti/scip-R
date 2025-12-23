#' @include utils.R
#' @include workspace.R
#' @include symbols.R
NULL

# This script handles serialization of SCIP data to the target output file.
# It prepares metadata, builds protobuf messages, and finalizes the index artifact.
# Future development should extend document population with full symbol data.

#' Initialize a serializer for SCIP output.
#'
#' @param output_path Desired location for the serialized `.scip` file.
#' @param workspace Workspace context providing metadata.
#' @return An environment acting as a serializer handle.
#' @export
#' @examples
#' serializer <- init_serializer("index.scip", analyze_workspace(tempdir()))
init_serializer <- function(output_path, workspace) {
  env <- new.env(parent = emptyenv())
  env$path <- output_path
  env$metadata <- workspace$metadata
  env$documents <- list()
  env$externals <- list()
  env$proto_path <- find_proto_path(workspace$metadata$project_root)
  env
}

#' Write a document payload through the serializer.
#'
#' @param serializer Serializer environment created by `init_serializer()`.
#' @param document Document list as returned by `build_document()`.
#' @param diagnostics Optional list of diagnostics to attach.
#' @return Invisible NULL.
#' @export
#' @examples
#' serializer <- init_serializer("index.scip", analyze_workspace(tempdir()))
#' write_document(serializer, list(relative_path = "script.R"))
write_document <- function(serializer, document, diagnostics = list()) {
  doc_message <- build_document_message(document, diagnostics)
  serializer$documents[[length(serializer$documents) + 1]] <- doc_message
  invisible(NULL)
}

#' Write external symbols through the serializer.
#'
#' @param serializer Serializer environment.
#' @param externals List of external symbol descriptors.
#' @return Invisible NULL.
#' @export
#' @examples
#' serializer <- init_serializer("index.scip", analyze_workspace(tempdir()))
#' write_externals(serializer, list())
write_externals <- function(serializer, externals) {
  if (length(externals) == 0) {
    serializer$externals <- list()
    return(invisible(NULL))
  }

  serializer$externals <- build_external_symbol_messages(externals)
  invisible(NULL)
}

#' Finalize and close the serializer.
#'
#' @param serializer Serializer environment.
#' @return Invisible path to the written file.
#' @export
#' @examples
#' serializer <- init_serializer("index.scip", analyze_workspace(tempdir()))
#' close_serializer(serializer)
close_serializer <- function(serializer) {
  index_msg <- build_index_message(serializer)
  raw <- RProtoBuf::serialize(index_msg, NULL)
  con <- file(serializer$path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(raw, con)
  invisible(serializer$path)
}

#' Locate the scip.proto schema file.
#'
#' Searches for scip.proto in the following order:
#' 1. Bundled with package (inst/scip.proto)
#' 2. SCIP_PROTO_PATH environment variable
#'
#' @param project_root Absolute path to the indexing project root (unused, kept for compatibility).
#' @return Normalized absolute path to scip.proto.
#' @keywords internal
find_proto_path <- function(project_root) {
  # Try bundled proto file (from inst/scip.proto)
  bundled <- system.file("scip.proto", package = "scipR")

  # Try environment variable override
  override <- Sys.getenv("SCIP_PROTO_PATH", unset = "")

  candidates <- c(bundled, override)
  candidates <- candidates[nzchar(candidates)]
  existing <- candidates[file.exists(candidates)]

  if (length(existing) == 0) {
    stop(
      "Unable to locate scip.proto schema file.\n\n",
      "Searched locations:\n",
      if (nzchar(bundled)) paste0("  - Package bundle: ", bundled, "\n"),
      if (nzchar(override)) paste0("  - SCIP_PROTO_PATH: ", override, "\n"),
      "\nSolutions:\n",
      "  1. Ensure scip.proto is present at: inst/scip.proto (for development)\n",
      "  2. Set SCIP_PROTO_PATH environment variable to the proto file location\n",
      "  3. Download from: https://github.com/sourcegraph/scip/blob/main/scip.proto",
      call. = FALSE
    )
  }

  proto_path <- normalizePath(existing[[1]], winslash = "/", mustWork = TRUE)
  if (!proto_loaded(proto_path)) {
    RProtoBuf::readProtoFiles2(proto_path)
  }
  proto_path
}

#' Check if a proto file has already been loaded in this session.
#'
#' Performance optimization to avoid re-parsing the same proto schema
#' multiple times within a single R session.
#'
#' @param proto_path Path to scip.proto.
#' @return Logical indicating whether descriptors are present.
#' @keywords internal
proto_loaded <- function(proto_path) {
  descriptor_name <- "scip.Index"
  # Check if descriptor exists in current session without triggering errors
  tryCatch({
    RProtoBuf::P(descriptor_name)
    TRUE
  }, error = function(e) {
    FALSE
  })
}

#' Construct a Document protobuf message from an internal document list.
#'
#' @param document Internal document representation.
#' @param diagnostics Optional diagnostics list.
#' @return A `scip.Document` message.
#' @keywords internal
build_document_message <- function(document, diagnostics) {
  doc_msg <- RProtoBuf::new(RProtoBuf::P("scip.Document"))
  doc_msg$language <- document$language %||% "R"
  doc_msg$relative_path <- document$relative_path

  if (!is.null(document$occurrences) && length(document$occurrences) > 0) {
    occ_msgs <- lapply(document$occurrences, build_occurrence_message)
    doc_msg$occurrences <- occ_msgs
  }

  if (!is.null(document$symbols) && length(document$symbols) > 0) {
    sym_msgs <- lapply(document$symbols, build_symbol_information_message)
    doc_msg$symbols <- sym_msgs
  }

  if (!is.null(diagnostics) && length(diagnostics) > 0) {
    diag_msgs <- lapply(diagnostics, build_diagnostic_occurrence)
    doc_msg$occurrences <- c(doc_msg$occurrences, diag_msgs)
  }

  doc_msg
}

#' Convert occurrence lists into protobuf messages.
#'
#' @param occurrence Occurrence entry from symbol analysis.
#' @return A `scip.Occurrence` message.
#' @keywords internal
build_occurrence_message <- function(occurrence) {
  occ <- RProtoBuf::new(RProtoBuf::P("scip.Occurrence"))
  occ$symbol <- occurrence$symbol
  occ$range <- as.integer(occurrence$range)
  occ$symbol_roles <- role_to_bitset(occurrence$role)
  occ
}

#' Convert diagnostic lists into placeholder occurrences.
#'
#' @param diagnostic Diagnostic entry produced by collectors.
#' @return A `scip.Occurrence` message with diagnostic data.
#' @keywords internal
build_diagnostic_occurrence <- function(diagnostic) {
  occ <- RProtoBuf::new(RProtoBuf::P("scip.Occurrence"))
  if (!is.null(diagnostic$range)) {
    occ$range <- as.integer(diagnostic$range)
  }
  diag_msg <- RProtoBuf::new(RProtoBuf::P("scip.Diagnostic"))
  diag_msg$message <- diagnostic$message %||% ""
  occ$diagnostics <- list(diag_msg)
  occ
}

#' Convert symbol information lists into protobuf messages.
#'
#' @param symbol_entry List describing the symbol.
#' @return A `scip.SymbolInformation` message.
#' @keywords internal
build_symbol_information_message <- function(symbol_entry) {
  sym <- RProtoBuf::new(RProtoBuf::P("scip.SymbolInformation"))
  sym$symbol <- symbol_entry$symbol
  sym$display_name <- symbol_entry$display_name %||% symbol_entry$symbol
  kind_enum <- RProtoBuf::P("scip.SymbolInformation.Kind")
  sym$kind <- switch(
    symbol_entry$kind,
    `function` = as.integer(kind_enum$Function),      # 17
    call = as.integer(kind_enum$Function),            # 17
    variable = as.integer(kind_enum$Variable),        # 61
    parameter = as.integer(kind_enum$Parameter),      # 37
    s3_generic = as.integer(kind_enum$Function),      # 17
    s3_method = as.integer(kind_enum$Method),         # 26
    r6_class = as.integer(kind_enum$Class),           # 7
    r6_method = as.integer(kind_enum$Method),         # 26
    r6_field = as.integer(kind_enum$Field),           # 15
    r6_constructor = as.integer(kind_enum$Constructor), # 9
    r6_active = as.integer(kind_enum$Property),       # 41
    as.integer(kind_enum$UnspecifiedKind)             # 0
  )
  sym
}

#' Convert external symbol placeholders into protobuf messages.
#'
#' @param externals List of external symbol descriptors.
#' @return A list of `scip.SymbolInformation` messages.
#' @keywords internal
build_external_symbol_messages <- function(externals) {
  msgs <- list()
  for (entry in externals) {
    if (is.null(entry$symbols) || length(entry$symbols) == 0) {
      next
    }
    for (sym_name in entry$symbols) {
      sym <- RProtoBuf::new(RProtoBuf::P("scip.SymbolInformation"))
      sym$symbol <- sym_name
      msgs[[length(msgs) + 1]] <- sym
    }
  }
  msgs
}

#' Compose the complete SCIP Index message.
#'
#' @param serializer Serializer environment with accumulated data.
#' @return A `scip.Index` protobuf message ready for serialization.
#' @keywords internal
build_index_message <- function(serializer) {
  index_msg <- RProtoBuf::new(RProtoBuf::P("scip.Index"))
  index_msg$metadata <- build_metadata_message(serializer$metadata)

  if (length(serializer$documents) > 0) {
    index_msg$documents <- serializer$documents
  }

  if (length(serializer$externals) > 0) {
    index_msg$external_symbols <- serializer$externals
  }

  index_msg
}

#' Build the Metadata protobuf message from workspace metadata.
#'
#' @param metadata Workspace metadata list.
#' @return A `scip.Metadata` protobuf message.
#' @keywords internal
build_metadata_message <- function(metadata) {
  meta_msg <- RProtoBuf::new(RProtoBuf::P("scip.Metadata"))
  meta_msg$version <- RProtoBuf::P("scip.ProtocolVersion")$UnspecifiedProtocolVersion

  tool_info <- RProtoBuf::new(RProtoBuf::P("scip.ToolInfo"))
  tool_info$name <- "scip-r"
  tool_info$version <- "0.0.1-dev"
  meta_msg$tool_info <- tool_info

  meta_msg$project_root <- metadata$project_uri %||% metadata$project_root
  meta_msg$text_document_encoding <- RProtoBuf::P("scip.TextEncoding")$UTF8
  meta_msg
}

#' Convert occurrence role into bitset flag.
#'
#' @param role Character role label.
#' @return Integer bitset according to SCIP SymbolRole enum.
#' @keywords internal
role_to_bitset <- function(role) {
  enum <- RProtoBuf::P("scip.SymbolRole")
  if (identical(role, "definition")) {
    return(as.integer(enum$Definition))
  }
  if (identical(role, "reference")) {
    return(as.integer(enum$ReadAccess))
  }
  as.integer(enum$UnspecifiedSymbolRole)
}
