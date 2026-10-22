#' Divide replications evenly across worker processes
#'
#' Splits `n_reps` replications among `n_cores` processes so that every process
#' gets the same base share and the remainder is handed out one replication at
#' a time, starting with the first process. This is the load-balancing step of
#' the general parallel simulation algorithm.
#'
#' @param n_reps Total number of replications.
#' @param n_cores Number of worker processes.
#'
#' @return An integer vector of length `n_cores` that sums to `n_reps`.
#' @export
#'
#' @examples
#' split_reps(1000003, 8)
#' sum(split_reps(1000003, 8))
split_reps <- function(n_reps, n_cores) {
  n_reps <- as.integer(n_reps)
  n_cores <- as.integer(n_cores)
  if (length(n_reps) != 1L || is.na(n_reps) || n_reps < 1L) {
    stop("`n_reps` must be a single positive integer.", call. = FALSE)
  }
  if (length(n_cores) != 1L || is.na(n_cores) || n_cores < 1L) {
    stop("`n_cores` must be a single positive integer.", call. = FALSE)
  }
  base_reps <- n_reps %/% n_cores
  remainder <- n_reps %% n_cores
  reps <- rep(base_reps, n_cores)
  reps[seq_len(remainder)] <- reps[seq_len(remainder)] + 1L
  reps
}

#' Build the per-process set-up lists
#'
#' Produces the list that [sim_template()] hands to each worker process: the
#' number of replications assigned to that process and the PCG64 seed and
#' stream that give it an independent random-number stream. The seed vectors
#' come from [dqrng::generateSeedVectors()], which draws from R's own generator,
#' so the master `seed` fixes every process's stream.
#'
#' Calling this function sets R's random number seed with `set.seed(seed)`,
#' exactly as the book's `sim_template` does.
#'
#' @param seed Master seed, passed to [set.seed()].
#' @param n_reps Total number of replications.
#' @param n_cores Number of worker processes.
#'
#' @return A list of length `n_cores`. Element `i` is a list with components
#'   `n_reps` (replications for process `i`), `stream` and `seed` (each a
#'   two-element integer vector for [dqrng::dqset.seed()]), `worker_id`
#'   (`i`), and `n_cores`.
#' @export
#'
#' @examples
#' set_up <- make_set_up(1234, 100, 4)
#' str(set_up[[1]])
make_set_up <- function(seed, n_reps, n_cores) {
  reps <- split_reps(n_reps, n_cores)
  n_cores <- length(reps)

  # set the seed for use with R's random number generator
  set.seed(seed)

  # integer seeds and increments for each process
  sv_list <- dqrng::generateSeedVectors(n_cores)
  stream_list <- dqrng::generateSeedVectors(n_cores)

  lapply(seq_len(n_cores), function(i) {
    list(
      n_reps = reps[i],
      stream = stream_list[[i]],
      seed = sv_list[[i]],
      worker_id = i,
      n_cores = n_cores
    )
  })
}

#' Seed the PCG64 generator inside a worker
#'
#' Every worker begins by seeding `dqrng` with the seed and stream it was
#' handed. This helper performs that call, so a worker can start with
#' `set_worker_rng(set_up)` instead of unpacking the two vectors by hand.
#'
#' @param set_up One element of the list returned by [make_set_up()].
#'
#' @return `set_up`, invisibly.
#' @export
#'
#' @examples
#' set_up <- make_set_up(1234, 100, 4)
#' set_worker_rng(set_up[[1]])
#' dqrng::dqrunif(3)
set_worker_rng <- function(set_up) {
  dqrng::dqRNGkind("pcg64")
  dqrng::dqset.seed(
    c(set_up$seed[[1]], set_up$seed[[2]]),
    c(set_up$stream[[1]], set_up$stream[[2]])
  )
  invisible(set_up)
}
