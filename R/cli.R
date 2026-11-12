# CLI installation utilities for scip-r.
# Installs or updates the `scip-r` launcher and keeps the PATH wrapper aligned.
# Inventory: `install_scip_cli()`.

NULL

#' Install or update the scip-r CLI executable
#'
#' Install the `scip-r` command explicitly after installing this package, or
#' reinstall it to a custom location. Loading the package never creates or
#' updates a launcher.
#'
#' @param destdir Destination directory for the CLI launcher. If `NULL`, Rapp
#'   chooses it: `RAPP_BIN_DIR`, then `XDG_BIN_HOME`, then `~/.local/bin`
#'   (`\%LOCALAPPDATA\%/Programs/R/Rapp/bin` on Windows).
#'
#' @return Invisibly returns `TRUE` when a launcher was written, `FALSE` otherwise.
#' @export
#' @examples
#' \dontrun{
#' # Install to default location (usually ~/.local/bin on Unix)
#' install_scip_cli()
#'
#' # Install to custom location
#' install_scip_cli(destdir = "~/my/custom/bin")
#' }
# BUG FIX: package loading used to overwrite the user's
# launcher with a temporary/check-library script and print startup messages.
# Registration now happens only through this explicit installer; the .onLoad
# writer is removed entirely, so arbitrary library copies are read-only too.
# Anti-regression tests: tests/testthat/test-cli-install.R
#   - loading scipR from normal-library leaves launchers untouched
#   - loading scipR from package.Rcheck leaves launchers untouched
#   - loading scipR from temporary-library leaves launchers untouched
install_scip_cli <- function(destdir = NULL) {
  # BUG FIX: the success message named RAPP_INSTALL_DIR or ~/.local/bin,
  # a variable Rapp never reads, so with RAPP_BIN_DIR or XDG paths set, or
  # on Windows, it pointed users at a directory without the launcher.
  # Report the path Rapp returns for the launcher it actually wrote.
  # Anti-regression tests: tests/testthat/test-cli-install.R
  #   - install message reports the launcher directory Rapp returned
  #   - install warns when Rapp writes no launcher
  created <- Rapp::install_pkg_cli_apps("scipR", destdir = destdir)
  created <- created[!is.na(created)]
  if (length(created) == 0L) {
    cli::cli_warn(c(
      "No {.code scip-r} launcher was written.",
      "i" = "An existing file with that name was kept; remove it or pass {.arg destdir}."
    ))
    return(invisible(FALSE))
  }

  install_location <- unique(dirname(created))
  cli::cli_alert_success("scip-r CLI tool installed to: {.path {install_location}}")
  cli::cli_alert_info("Make sure this directory is in your PATH")

  invisible(TRUE)
}
