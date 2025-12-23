# This script configures testthat execution for the scip-r prototype.
# It ensures all module scripts are available before running expectations.
# Future iterations should migrate to a formal package structure.

if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Package 'testthat' is required to run scip-r tests.")
}

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
test_root <- if (length(file_arg) > 0) {
  dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
} else {
  getwd()
}

testthat::test_dir(file.path(test_root, "testthat"))
