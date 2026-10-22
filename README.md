# simtemplate

<!-- badges: start -->
[![R-CMD-check](https://github.com/ismayc/simtemplate/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ismayc/simtemplate/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Run simulation experiments in parallel on a multicore personal computer.

`simtemplate` generalizes the `sim_template` program developed in the book
*Parallel Simulation Experiments on a Personal Computer* by R. L. Eubank and
Chester Ismay (CRC Press). The book's template reads its worker from a file
named `sim_worker.R` in the working directory. In the package the worker is an
argument, so any number of workers can live side by side, and the pieces of
the template are available on their own:

- `sim_template()` runs a worker across `n_cores` processes, each with its own
  independent PCG64 stream from the `dqrng` package.
- `sim_template_serial()` runs the same experiment in the current process, for
  debugging a worker and for the one-core baseline of a timing study.
- `make_set_up()`, `split_reps()`, and `set_worker_rng()` are the building
  blocks: the per-process seeds and streams, the even division of the
  replications, and the worker-side seeding call.
- `pi_worker()`, `iqr_worker()`, and `regression_worker()` are the book's
  example workers (Chapters 3 and 5).
- `benchmark_cores()` times a simulation across core counts and reports the
  speed-up (Chapter 7).

The companion Python package is
[simtemplate-python](https://github.com/ismayc/simtemplate-python). The book's
code repository is
[parallel-simulation-book](https://github.com/ismayc/parallel-simulation-book),
and the web edition is at
<https://parallel-simulation-book.netlify.app>.

## Installation

``` r
# install.packages("remotes")
remotes::install_github("ismayc/simtemplate")
```

## Usage

The call mirrors the book: the seed, the sample size for each replication,
the number of replications, the number of processes, and then any model
parameters. The one difference is that the worker comes first.

``` r
library(simtemplate)

# Chapter 3: estimate pi with 1,000,000 replications of 100 darts on 4 cores
out <- sim_template(pi_worker, 1234, 100, 1000000, 4)
mean(bind_results(out))
#> [1] 3.141554

# Chapter 3: the Cauchy scale estimator, with theta = 5 passed through `...`
out <- sim_template(iqr_worker, 1234, 100, 20, 4, 5)
lapply(out, round, 3)
#> [[1]]
#> [1] 5.930 4.992 4.470 4.336 4.306
#> ...
```

Those are the numbers printed in the book. The package's tests pin them, so
a change that alters a published result fails the test suite.

### Writing your own worker

A worker is a function `worker(n, set_up, ...)`. It seeds its generator from
`set_up`, generates the data for its `set_up$n_reps` replications, computes
the quantity of interest, and returns it. Keep it self-contained: it runs in a
fresh process that has never seen your workspace.

``` r
mean_worker <- function(n, set_up, ...) {
  set_worker_rng(set_up)              # the process's own PCG64 stream
  mu <- list(...)[[1]]                # model parameters arrive through ...
  x <- matrix(dqrng::dqrnorm(n * set_up$n_reps, mean = mu), nrow = n)
  colMeans(x)                         # one estimate per replication
}

out <- sim_template(mean_worker, 2026, 50, 10000, 4, 10)
summary(bind_results(out))
```

A worker that calls a package's functions without the `pkg::` prefix needs
that package attached on every process: `sim_template(..., packages =
"matrixStats")`.

### Timing a speed-up experiment

``` r
run_pi <- function(n_cores) sim_template(pi_worker, 1234, 100, 1000000, n_cores)
benchmark_cores(run_pi, cores = 1:8, n_runs = 10)
```

## Reproducibility

A fixed `seed` reproduces a run exactly on the same number of cores. Changing
`n_cores` changes how the replications are divided across the streams, so the
full quartet `seed`, `n`, `n_reps`, and `n_cores` is what reproduces a result.

## License

MIT. See [LICENSE.md](LICENSE.md).
