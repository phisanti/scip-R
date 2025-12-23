#' @include parser.R
#' @include symbols.R
#' @include occurrences.R
#' @include diagnostics.R
NULL

# This script introduces mirai-powered parallel symbol resolution for documents.
# It evaluates parsing, symbol extraction, and document construction concurrently.
# Future steps should reuse persistent daemons across indexing runs for amortized startup.

#' Resolve documents in parallel using mirai daemons.
#'
#' @param files List of file descriptors from `discover_files()`.
#' @param workspace Workspace context obtained via `analyze_workspace()`.
#' @param daemons Number of mirai daemons to launch (defaults to physical cores).
#' @return A list of lists, each containing `document` and `diagnostics` entries.
#' @export
#' @examples
#' \dontrun{
#' docs <- resolve_documents_mirai(plan$files, workspace, daemons = 4)
#' }
resolve_documents_mirai <- function(files, workspace, daemons = parallel::detectCores(logical = FALSE)) {
  if (!requireNamespace("mirai", quietly = TRUE)) {
    stop("Package 'mirai' is required for parallel resolution.\n",
         "Please install it via: install.packages('mirai')",
         call. = FALSE)
  }
  daemons <- max(1L, daemons)
  mirai::daemons(daemons)
  on.exit(mirai::daemons(0), add = TRUE)

  tasks <- lapply(files, function(file_info) {
    mirai::mirai(
      {
        library(scipR)
        ast <- parse_file(file_info)
        analysis <- resolve_symbols(ast, workspace, track_externals = TRUE)
        occurrences <- map_occurrences(analysis, ast)
        document <- build_document(file_info, occurrences, analysis)
        diagnostics <- collect_diagnostics(file_info, ast, analysis)
        list(document = document, diagnostics = diagnostics, external_refs = analysis$external_refs)
      },
      file_info = file_info,
      workspace = workspace
    )
  })

  lapply(tasks, function(task) mirai::collect_mirai(task))
}
