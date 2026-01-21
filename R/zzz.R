#' Package initialization hook
#'
#' Registers the scip-r CLI tool via Rapp on every package load.
#'
#' @param libname Library name (passed by R)
#' @param pkgname Package name (passed by R)
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Skip if we're in a LOCK directory (temporary installation location)
  pkg_dir <- system.file(package = pkgname, lib.loc = libname)
  if (grepl("00LOCK", pkg_dir)) {
    return(invisible(NULL))
  }

  # Always register CLI via Rapp - it's idempotent and ensures wrapper points to current install

  tryCatch(
    Rapp::install_pkg_cli_apps(pkgname),
    error = function(e) invisible(NULL)
  )
}
