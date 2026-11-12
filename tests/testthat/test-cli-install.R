# Launcher installation is explicit; namespace loads must be read-only.
# Capture the real initialization hook from the package namespace,
# while intercepting Rapp at its boundary so the user launcher is never touched.
for (context in c("normal-library", "package.Rcheck", "temporary-library")) {
  local({
    load_context <- context
    test_that(sprintf("loading scipR from %s leaves launchers untouched", load_context), {
      tmp <- tempfile("scipr_load_")
      dir.create(tmp)
      on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
      launcher <- file.path(tmp, "scip-r")
      writeLines("existing user launcher", launcher)
      calls <- 0L
      testthat::local_mocked_bindings(
        install_pkg_cli_apps = function(...) {
          calls <<- calls + 1L
          writeLines("overwritten by temporary package", launcher)
        },
        .package = "Rapp"
      )
      library_path <- file.path(tmp, load_context)
      # Removing a write-only load hook is the fix. If a hook is reintroduced,
      # the same tests execute it and catch any registration side effect.
      ns <- asNamespace("scipR")
      if (exists(".onLoad", envir = ns, inherits = FALSE)) {
        hook <- get(".onLoad", envir = ns)
        # Scope only filesystem resolution; retain the production hook body.
        environment(hook) <- new.env(parent = environment(hook))
        environment(hook)$system.file <- function(...) file.path(library_path, "scipR")
        hook(library_path, "scipR")
      }
      expect_equal(calls, 0L)
      expect_identical(readLines(launcher), "existing user launcher")
    })
  })
}

test_that("explicit CLI installation preserves default and custom destinations", {
  calls <- list()
  testthat::local_mocked_bindings(
    install_pkg_cli_apps = function(package, destdir = NULL, ...) {
      calls[[length(calls) + 1L]] <<- list(package = package, destdir = destdir)
      file.path(destdir %||% "default launcher directory", "scip-r")
    },
    .package = "Rapp"
  )
  suppressMessages({
    expect_true(install_scip_cli())
    expect_true(install_scip_cli(destdir = "custom launcher directory"))
  })
  expect_identical(calls, list(
    list(package = "scipR", destdir = NULL),
    list(package = "scipR", destdir = "custom launcher directory")
  ))
})

test_that("install message reports the launcher directory Rapp returned", {
  # The old message fell back to ~/.local/bin whenever this was unset.
  old_dir <- Sys.getenv("RAPP_INSTALL_DIR", unset = NA)
  Sys.unsetenv("RAPP_INSTALL_DIR")
  on.exit(if (!is.na(old_dir)) Sys.setenv(RAPP_INSTALL_DIR = old_dir), add = TRUE)
  testthat::local_mocked_bindings(
    install_pkg_cli_apps = function(package, destdir = NULL, ...) "/rapp/bin/dir/scip-r",
    .package = "Rapp"
  )
  msgs <- character()
  withCallingHandlers(
    install_scip_cli(),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  msgs <- paste(msgs, collapse = "\n")
  expect_match(msgs, "/rapp/bin/dir", fixed = TRUE)
  expect_no_match(msgs, ".local/bin", fixed = TRUE)
})

test_that("install warns when Rapp writes no launcher", {
  testthat::local_mocked_bindings(
    install_pkg_cli_apps = function(package, destdir = NULL, ...) character(),
    .package = "Rapp"
  )
  expect_warning(result <- install_scip_cli(destdir = "kept"), "No .*launcher was written")
  expect_false(result)
})
