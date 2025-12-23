#' Package initialization hook
#'
#' Automatically installs the scip-r CLI tool once during package installation.
#' Uses a marker file to ensure it only runs during installation, not on every load.
#'
#' @param libname Library name (passed by R)
#' @param pkgname Package name (passed by R)
#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Check if CLI has already been installed using a marker file
  pkg_dir <- system.file(package = pkgname, lib.loc = libname)
  marker_file <- file.path(pkg_dir, ".cli_installed")
  
  # Skip if we're in a LOCK directory (temporary installation location)
  if (grepl("00LOCK", pkg_dir)) {
    return(invisible(NULL))
  }
  
  # Only install if marker doesn't exist (i.e., first time during installation)
  if (!file.exists(marker_file)) {
    tryCatch({
      # Install CLI tools via Rapp
      Rapp::install_pkg_cli_apps(pkgname)
      
      # Create marker file to prevent re-installation on subsequent loads
      dir.create(dirname(marker_file), showWarnings = FALSE, recursive = TRUE)
      writeLines(as.character(Sys.time()), marker_file)
    }, error = function(e) {
      # Silently fail - users can manually call install_scip_cli() if needed
      invisible(NULL)
    })
  }
}
