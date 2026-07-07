# CLI installation utilities for scip-r.
# Installs or updates the `scip-r` launcher and keeps the PATH wrapper aligned.
# Inventory: `install_scip_cli()`.

NULL

#' Install or update the scip-r CLI executable
#'
#' Normally, the `scip-r` command is installed automatically when the
#' package loads. Use this function if you need to reinstall the CLI tool or
#' install it to a custom location.
#'
#' @param destdir Destination directory for the CLI launcher.
#'                If `NULL`, uses Rapp's default behavior (respects
#'                `RAPP_INSTALL_DIR` environment variable and standard
#'                XDG paths like `~/.local/bin`).
#'
#' @return Invisibly returns `TRUE` on success.
#' @export
#' @examples
#' \dontrun{
#' # Install to default location (usually ~/.local/bin on Unix)
#' install_scip_cli()
#'
#' # Install to custom location
#' install_scip_cli(destdir = "~/my/custom/bin")
#' }
install_scip_cli <- function(destdir = NULL) {
  if (!requireNamespace("Rapp", quietly = TRUE)) {
    cli::cli_abort(c(
      "{.pkg Rapp} package required for CLI installation.",
      "i" = "Install with: {.code remotes::install_github('r-lib/Rapp')}"
    ))
  }

  if (is.null(destdir)) {
    Rapp::install_pkg_cli_apps("scipR")
    # Determine default installation location
    # Rapp typically uses ~/.local/bin on Unix, or respects RAPP_INSTALL_DIR
    install_location <- Sys.getenv("RAPP_INSTALL_DIR", "~/.local/bin")
    install_location <- path.expand(install_location)
  } else {
    Rapp::install_pkg_cli_apps("scipR", destdir = destdir)
    install_location <- destdir
  }

  cli::cli_alert_success("scip-r CLI tool installed to: {.path {install_location}}")
  cli::cli_alert_info("Make sure this directory is in your PATH")

  invisible(TRUE)
}
