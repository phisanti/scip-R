# Launcher installation is explicit; namespace loads must be read-only.
# Capture the real initialization hook through the prototype source loader,
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
      if (exists(".onLoad", envir = globalenv(), inherits = FALSE)) {
        hook <- get(".onLoad", envir = globalenv())
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
    },
    .package = "Rapp"
  )
  expect_true(install_scip_cli())
  expect_true(install_scip_cli(destdir = "custom launcher directory"))
  expect_identical(calls, list(
    list(package = "scipR", destdir = NULL),
    list(package = "scipR", destdir = "custom launcher directory")
  ))
})
