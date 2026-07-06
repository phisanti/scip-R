#' @include utils.R
#' @include symbols_mirai.R
#' @include serializer.R
NULL

# This script combines mirai-based document preparation with final serialization.
# It parallelises parsing and symbol extraction, then streams results into the serializer.
# Future revisions might pipeline protobuf writes directly from mirai workers to shared buffers.

#' Create a SCIP index using mirai-prepared documents.
#'
#' @param files List of file descriptors as returned by `discover_files()`.
#' @param workspace Workspace list from `analyze_workspace()`.
#' @param output_path Destination `.scip` path.
#' @param daemons Number of mirai daemons for document preparation.
#' @param bundles Optional pre-computed document bundles.
#' @return Invisibly returns the output path.
#' @keywords internal
#' @examples
#' \dontrun{
#' serialize_documents_mirai(plan$files, workspace, "index.scip", daemons = 4)
#' }
serialize_documents_mirai <- function(files, workspace, output_path,
                                      daemons = parallel::detectCores(logical = FALSE),
                                      bundles = NULL) {
  bundles <- bundles %||% resolve_documents_mirai(files, workspace, daemons = daemons)
  serializer <- init_serializer(output_path, workspace)
  on.exit(
    {
      if (!is.null(serializer)) close_serializer(serializer)
    },
    add = TRUE
  )

  # Accumulate external references across all bundles
  all_external_refs <- list()

  for (bundle in bundles) {
    write_document(serializer, bundle$document, diagnostics = bundle$diagnostics)

    # Merge external refs if present in bundle
    if (!is.null(bundle$external_refs) && length(bundle$external_refs) > 0) {
      all_external_refs <- merge_external_refs(all_external_refs, bundle$external_refs)
    }
  }

  write_externals(serializer, all_external_refs)

  result <- close_serializer(serializer)
  serializer <- NULL
  invisible(result)
}
