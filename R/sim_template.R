#' Run a simulation experiment in parallel
#'
#' The general parallel simulation algorithm from the book, with the worker
#' passed as a function instead of read from a file named `sim_worker.R`:
#' choose a master seed, give each process an independent PCG64 stream,
#' divide the replications evenly, run the worker on every process, and
#' return the combined results.
#'
#' A worker is a function `worker(n, set_up, ...)`. `n` is the per-replication
#' sample size, `set_up` is one element of [make_set_up()] (the process's
#' `n_reps`, `seed`, and `stream`), and `...` carries any model parameters
#' given to `sim_template()`. The worker must seed its generator from
#' `set_up`, for example with [set_worker_rng()], and must be self-contained:
#' it runs in a fresh process that has not seen your workspace. See
#' [pi_worker()], [iqr_worker()], and [regression_worker()] for the book's
#' examples.
#'
#' Results are reproducible for a fixed `seed` **and** a fixed `n_cores`.
#' Changing the number of processes changes how the replications are divided
#' and therefore which stream produces which draw.
#'
#' @param worker A worker function `worker(n, set_up, ...)`.
#' @param seed Master seed.
#' @param n Sample size for each replication.
#' @param n_reps Total number of replications.
#' @param n_cores Number of worker processes.
#' @param ... Model parameters passed on to `worker`.
#' @param packages Character vector of packages to attach on each process in
#'   addition to `dqrng`, for workers that call functions without a `pkg::`
#'   prefix.
#' @param type Cluster type passed to [parallel::makeCluster()]. The default
#'   `"PSOCK"` works everywhere. `"FORK"` is available on macOS and Linux.
#' @param cl An existing cluster from [parallel::makeCluster()]. When
#'   supplied, it is used as is and not stopped; `n_cores` must equal its
#'   size.
#'
#' @return A list of length `n_cores` holding each process's result, in
#'   process order. Use [bind_results()] to combine them.
#' @export
#'
#' @examples
#' out <- sim_template(pi_worker, 1234, 100, 1000, 2)
#' mean(bind_results(out))
#'
#' # the Cauchy scale example, with the scale parameter passed through `...`
#' out <- sim_template(iqr_worker, 1234, 100, 20, 2, 5)
#' lapply(out, round, 3)
sim_template <- function(worker, seed, n, n_reps, n_cores, ...,
                         packages = NULL, type = "PSOCK", cl = NULL) {
  if (!is.function(worker)) {
    stop("`worker` must be a function of the form worker(n, set_up, ...).",
         call. = FALSE)
  }

  # seeds, streams, and replication counts for each process
  set_up <- make_set_up(seed, n_reps, n_cores)
  n_cores <- length(set_up)

  # create the cluster, unless the caller supplied one
  if (is.null(cl)) {
    cl <- parallel::makeCluster(n_cores, type = type)
    on.exit(parallel::stopCluster(cl), add = TRUE)
  } else if (length(cl) != n_cores) {
    stop("`cl` has ", length(cl), " nodes but `n_cores` is ", n_cores, ".",
         call. = FALSE)
  }

  # every process uses PCG64 through dqrng, and can call this package's
  # helpers (set_worker_rng) without a prefix
  parallel::clusterEvalQ(cl, {
    library(dqrng)
    dqrng::dqRNGkind("pcg64")
    if (requireNamespace("simstreams", quietly = TRUE)) {
      library(simstreams)
    }
  })
  for (pkg in packages) {
    parallel::clusterCall(cl, function(p) library(p, character.only = TRUE),
                          pkg)
  }

  # apply the worker to each process's set_up
  parallel::parLapply(
    cl, set_up,
    function(worker_set_up, worker, n, ...) worker(n, worker_set_up, ...),
    worker = worker, n = n, ...
  )
}

#' Run a simulation experiment in a single process
#'
#' The serial counterpart of [sim_template()]: the same seed and stream layout
#' as a one-process run, executed in the current R session with no cluster.
#' Useful for debugging a worker before running it in parallel, and for the
#' one-core baseline of a timing experiment. The result equals
#' `sim_template(worker, seed, n, n_reps, 1, ...)[[1]]`.
#'
#' @inheritParams sim_template
#'
#' @return The worker's result for all `n_reps` replications.
#' @export
#'
#' @examples
#' theta_hat <- sim_template_serial(iqr_worker, 1234, 100, 20, 5)
#' round(theta_hat, 3)
sim_template_serial <- function(worker, seed, n, n_reps, ...) {
  if (!is.function(worker)) {
    stop("`worker` must be a function of the form worker(n, set_up, ...).",
         call. = FALSE)
  }
  set_up <- make_set_up(seed, n_reps, 1L)
  dqrng::dqRNGkind("pcg64")
  worker(n, set_up[[1]], ...)
}

#' Combine per-process results
#'
#' [sim_template()] returns one element per process. This helper joins them:
#' vectors are concatenated, and matrices with replications in columns (as
#' [regression_worker()] returns) are bound column-wise.
#'
#' @param result The list returned by [sim_template()].
#'
#' @return A vector or a matrix covering all replications.
#' @export
#'
#' @examples
#' out <- sim_template(pi_worker, 1234, 100, 1000, 2)
#' length(bind_results(out))
bind_results <- function(result) {
  if (all(vapply(result, is.matrix, logical(1)))) {
    return(do.call(cbind, result))
  }
  unlist(result, use.names = FALSE)
}
