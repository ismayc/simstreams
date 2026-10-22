test_that("sim_template_serial equals a one-process parallel run", {
  skip_on_cran()
  serial <- sim_template_serial(iqr_worker, 1234, 100, 20, 5)
  parallel_1 <- sim_template(iqr_worker, 1234, 100, 20, 1, 5)
  expect_identical(serial, parallel_1[[1]])
  expect_length(serial, 20)
})

test_that("results are reproducible for a fixed seed and core count", {
  skip_on_cran()
  a <- sim_template(pi_worker, 42, 50, 40, 2)
  b <- sim_template(pi_worker, 42, 50, 40, 2)
  expect_identical(a, b)
  expect_equal(lengths(a), c(20L, 20L))
})

test_that("a user-written worker receives n, set_up, and the parameters", {
  skip_on_cran()
  mean_worker <- function(n, set_up, ...) {
    set_worker_rng(set_up)
    mu <- list(...)[[1]]
    x <- matrix(dqrng::dqrnorm(n * set_up$n_reps, mean = mu), nrow = n)
    c(worker_id = set_up$worker_id, mean = mean(colMeans(x)))
  }
  out <- sim_template(mean_worker, 7, 30, 10, 2, 100)
  expect_equal(out[[1]][["worker_id"]], 1)
  expect_equal(out[[2]][["worker_id"]], 2)
  expect_equal(out[[1]][["mean"]], 100, tolerance = 0.1)
})

test_that("an existing cluster can be reused and is not stopped", {
  skip_on_cran()
  cl <- parallel::makeCluster(2)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  a <- sim_template(pi_worker, 1234, 100, 20, 2, cl = cl)
  b <- sim_template(pi_worker, 1234, 100, 20, 2)
  expect_identical(a, b)
  expect_error(sim_template(pi_worker, 1234, 100, 20, 3, cl = cl), "nodes")
})

test_that("packages are attached on the workers", {
  skip_on_cran()
  stats_worker <- function(n, set_up, ...) {
    set_worker_rng(set_up)
    colQuantiles(matrix(dqrunif(n * set_up$n_reps), nrow = n), probs = 0.5)
  }
  expect_error(sim_template(stats_worker, 1, 10, 4, 2))
  out <- sim_template(stats_worker, 1, 10, 4, 2, packages = "matrixStats")
  expect_equal(lengths(out), c(2L, 2L))
})

test_that("bind_results concatenates vectors and binds matrices by column", {
  expect_equal(bind_results(list(1:2, 3:4)), 1:4)
  m <- bind_results(list(matrix(1:6, 3), matrix(7:9, 3)))
  expect_equal(dim(m), c(3L, 3L))
  expect_equal(m[, 3], 7:9)
})

test_that("worker must be a function", {
  expect_error(sim_template("pi_worker", 1, 10, 4, 2), "function")
  expect_error(sim_template_serial("pi_worker", 1, 10, 4), "function")
})

test_that("benchmark_cores reports medians, speed-up, and efficiency", {
  skip_on_cran()
  run_pi <- function(n_cores) sim_template(pi_worker, 1234, 100, 200, n_cores)
  bm <- benchmark_cores(run_pi, cores = 1:2, n_runs = 2, warmup = 0,
                        verbose = FALSE)
  expect_equal(bm$cores, 1:2)
  expect_equal(bm$speedup[1], 1)
  expect_equal(bm$efficiency, bm$speedup / bm$cores)
  expect_true(all(bm$median > 0))
  expect_length(attr(bm, "times"), 2)
  expect_length(attr(bm, "times")[[1]], 2)
})

test_that("benchmark_cores warms up, reports progress, and checks its input", {
  calls <- 0
  run_fake <- function(n_cores) calls <<- calls + 1
  expect_output(
    bm <- benchmark_cores(run_fake, cores = 1, n_runs = 2, warmup = 1),
    "Benchmarking 1 core"
  )
  expect_equal(calls, 3)  # one warm-up run plus two timed runs
  expect_equal(nrow(bm), 1)
  expect_error(benchmark_cores("run_pi"), "function")
})

test_that("the per-process helpers work in the current process too", {
  # init_worker attaches dqrng and selects PCG64
  expect_true(init_worker())
  expect_true("package:dqrng" %in% search())

  # attach_package attaches a package by name
  expect_equal(attach_package("matrixStats"), "matrixStats")
  expect_true("package:matrixStats" %in% search())

  # run_worker calls the worker on one process's set_up
  set_up <- make_set_up(1234, 20, 1)
  expect_identical(run_worker(set_up[[1]], iqr_worker, 100, 5),
                   sim_template_serial(iqr_worker, 1234, 100, 20, 5))
})

test_that("every book worker runs in the current process", {
  pi_hat <- sim_template_serial(pi_worker, 1234, 100, 10)
  expect_length(pi_hat, 10)
  expect_true(all(pi_hat >= 0 & pi_hat <= 4))

  theta_hat <- sim_template_serial(iqr_worker, 1234, 100, 10, 5)
  expect_length(theta_hat, 10)

  reg <- sim_template_serial(regression_worker, 1234, 50, 2, .5, .25, 1, 1.5, 5)
  expect_equal(dim(reg), c(3L, 2L))
  expect_equal(rownames(reg), c("theta_hat", "lower", "upper"))
  expect_true(all(reg["lower", ] <= reg["upper", ]))
})
