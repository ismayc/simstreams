# R CMD check --as-cran sets _R_CHECK_LIMIT_CORES_=TRUE, which makes
# parallel::makeCluster() refuse more than 2 processes. The book's printed
# results were produced with 4 cores, so those pins can only run where the
# limit is off (a plain test run, or CI with the variable set to "false").
skip_if_core_limited <- function(n_cores) {
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && chk != "false" && n_cores > 2L) {
    testthat::skip(paste0(
      "needs ", n_cores, " cores but _R_CHECK_LIMIT_CORES_ is set"
    ))
  }
  invisible(TRUE)
}
