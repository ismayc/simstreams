#' simtemplate: Parallel Simulation Experiments on a Personal Computer
#'
#' The package generalizes the `sim_template` program developed in the book
#' *Parallel Simulation Experiments on a Personal Computer* by R. L. Eubank
#' and Chester Ismay (CRC Press). The book's templates read the worker from a
#' file named `sim_worker.R`; here the worker is an argument, so any number of
#' workers can live side by side, and the pieces of the template (dividing
#' the replications, building the per-process seeds and streams, timing a
#' speed-up experiment) are available on their own.
#'
#' The main functions are:
#'
#' * [sim_template()] runs a worker across `n_cores` processes with one
#'   independent PCG64 stream per process.
#' * [sim_template_serial()] runs the same experiment in the current process.
#' * [make_set_up()], [split_reps()], and [set_worker_rng()] are the
#'   building blocks, for writing your own driver.
#' * [pi_worker()], [iqr_worker()], and [regression_worker()] are the book's
#'   example workers.
#' * [benchmark_cores()] times a simulation across core counts.
#'
#' The companion Python package is `simtemplate` on PyPI
#' (<https://github.com/ismayc/simtemplate-python>). The book's code
#' repository is <https://github.com/ismayc/parallel-simulation-book>, and the
#' web edition is at <https://parallel-simulation-book.netlify.app>.
#'
#' @keywords internal
"_PACKAGE"
