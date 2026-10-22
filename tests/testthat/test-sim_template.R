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
