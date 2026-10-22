#' Time a simulation across core counts
#'
#' Runs `run_case(n_cores)` for each core count, records the elapsed wall-clock
#' time of each run, and reports the median and spread together with the
#' speed-up and efficiency relative to the first core count. This is the
#' timing experiment behind the tables in Chapters 3 to 5 and the advice in
#' Chapter 7: time the whole call, including cluster start-up, discard
#' warm-up runs, and summarize with the median.
#'
#' @param run_case A function of one argument, `n_cores`, that runs the
#'   complete simulation and returns its result. Typically a thin wrapper
#'   around [sim_template()] with every other argument fixed.
#' @param cores Integer vector of core counts to time.
#' @param n_runs Number of timed runs at each core count.
#' @param warmup Number of untimed runs at each core count before timing
#'   starts.
#' @param verbose Print progress to the console.
#'
#' @return A data frame with one row per core count and columns `cores`,
#'   `median`, `iqr`, `min`, `max` (seconds), `speedup` (first-row median over
#'   this row's median), and `efficiency` (`speedup / cores`). The individual
#'   run times are attached as the `"times"` attribute, a list of numeric
#'   vectors.
#' @export
#'
#' @examples
#' \donttest{
#' run_pi <- function(n_cores) sim_template(pi_worker, 1234, 100, 20000, n_cores)
#' benchmark_cores(run_pi, cores = 1:2, n_runs = 2, warmup = 0)
#' }
benchmark_cores <- function(run_case, cores = 1:8, n_runs = 10, warmup = 1,
                            verbose = TRUE) {
  if (!is.function(run_case)) {
    stop("`run_case` must be a function of one argument, `n_cores`.",
         call. = FALSE)
  }

  times <- vector("list", length(cores))
  rows <- vector("list", length(cores))

  for (i in seq_along(cores)) {
    n_cores <- cores[i]
    if (verbose) cat("Benchmarking", n_cores, "core(s)\n")

    for (w in seq_len(warmup)) invisible(run_case(n_cores))

    elapsed <- vapply(seq_len(n_runs), function(run) {
      system.time(run_case(n_cores))[["elapsed"]]
    }, numeric(1))

    times[[i]] <- elapsed
    rows[[i]] <- data.frame(
      cores = n_cores,
      median = stats::median(elapsed),
      iqr = stats::IQR(elapsed),
      min = min(elapsed),
      max = max(elapsed)
    )
  }

  results <- do.call(rbind, rows)
  results$speedup <- results$median[1] / results$median
  results$efficiency <- results$speedup / results$cores
  attr(results, "times") <- times
  results
}
