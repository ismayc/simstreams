# These tests pin the package to the numbers printed in the book, so that a
# change to the template or a worker that alters a published result fails here.
# The values come from the book repository's verify/verify.R.

test_that("Chapter 3 pi: sim_template(1234, 100, 1e6, 4) prints 3.141554", {
  skip_on_cran()
  out <- sim_template(pi_worker, 1234, 100, 1000000, 4)
  expect_length(out, 4)
  expect_equal(round(mean(bind_results(out)), 6), 3.141554)
})

test_that("Chapter 3 Cauchy: sim_template(1234, 100, 20, 4, 5)", {
  skip_on_cran()
  out <- sim_template(iqr_worker, 1234, 100, 20, 4, 5)
  expect_equal(
    lapply(out, function(v) round(unname(v), 6)),
    list(c(5.929604, 4.991993, 4.470474, 4.336315, 4.306422),
         c(4.626911, 6.108323, 4.542509, 4.707605, 4.394840),
         c(5.243493, 5.121492, 4.692002, 4.361764, 3.357274),
         c(4.280723, 7.402930, 5.367711, 5.099618, 5.264122))
  )
  # Appendix A: lapply(out, round, 3)
  expect_equal(
    lapply(out, function(v) round(unname(v), 3)),
    list(c(5.930, 4.992, 4.470, 4.336, 4.306),
         c(4.627, 6.108, 4.543, 4.708, 4.395),
         c(5.243, 5.121, 4.692, 4.362, 3.357),
         c(4.281, 7.403, 5.368, 5.100, 5.264))
  )
})

test_that("the package workers match the book's sim_worker.R files exactly", {
  skip_on_cran()
  # Run the book's own template with its own worker file, from a temp dir,
  # the same way verify/verify.R does, and compare with the package.
  book_template <- function(worker_src, seed, n, n_reps, n_cores, ...) {
    td <- tempfile("simbook")
    dir.create(td)
    on.exit(unlink(td, recursive = TRUE), add = TRUE)
    writeLines(worker_src, file.path(td, "sim_worker.R"))
    old <- setwd(td)
    on.exit(setwd(old), add = TRUE)
    book_sim_template(seed, n, n_reps, n_cores, ...)
  }

  # Chapter 3 listing of sim_template.R
  book_sim_template <- function(seed, n, n_reps, n_cores, ...) {
    source("sim_worker.R", local = TRUE)
    set.seed(seed)
    base_reps <- n_reps %/% n_cores
    remainder <- n_reps %% n_cores
    reps <- rep(base_reps, n_cores)
    for (i in seq_len(remainder)) reps[i] <- reps[i] + 1
    sv_list <- dqrng::generateSeedVectors(n_cores)
    stream_list <- dqrng::generateSeedVectors(n_cores)
    set_up <- vector("list", n_cores)
    for (i in 1:n_cores) {
      set_up[[i]] <- list(n_reps = reps[i], stream = stream_list[[i]],
                          seed = sv_list[[i]])
    }
    cl <- parallel::makeCluster(n_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterExport(cl, c("worker", "n"), envir = environment())
    parallel::clusterEvalQ(cl, library(dqrng))
    parallel::clusterEvalQ(cl, dqRNGkind("pcg64"))
    parallel::parLapply(cl, set_up, function(worker_setup) {
      worker(n, worker_setup, ...)
    })
  }

  # Chapter 3 listing of IQR_worker.R
  iqr_src <- '
worker <- function(n, set_up, ...){
  library(matrixStats)
  n_reps <- set_up$n_reps
  stream_1 <- set_up$stream[1]
  stream_2 <- set_up$stream[2]
  seed_1 <- set_up$seed[1]
  seed_2 <- set_up$seed[2]
  param_list <- list(...)
  theta <- param_list[[1]]
  dqset.seed(c(seed_1, seed_2), c(stream_1, stream_2))
  U <- matrix(dqrunif(n * n_reps), nrow = n, ncol =  n_reps)
  X <- theta * tan(pi*(U - .5))
  Q <- matrixStats::colQuantiles(X, probs = c(0.25, 0.75), type = 7)
  theta_hat <- 0.5 * (Q[, 2] - Q[, 1])
  theta_hat
}'
  expect_identical(
    sim_template(iqr_worker, 1234, 100, 20, 2, 5),
    book_template(iqr_src, 1234, 100, 20, 2, 5)
  )

  # Chapter 5 listing of regression_worker.R
  reg_src <- '
worker <- function(n, set_up, ...) {
  n_reps   <- set_up$n_reps
  stream_1 <- set_up$stream[[1]]
  stream_2 <- set_up$stream[[2]]
  seed_1   <- set_up$seed[[1]]
  seed_2   <- set_up$seed[[2]]
  dqset.seed(c(seed_1, seed_2), c(stream_1, stream_2))
  param_list <- list(...)
  theta  <- param_list[[1]]
  sigma  <- param_list[[2]]
  beta_1 <- param_list[[3]]
  beta_2 <- param_list[[4]]
  B      <- param_list[[5]]
  epsilon <- matrix(dqrnorm(n * n_reps, mean = 0, sd = sigma),
                    nrow = n, ncol = n_reps)
  t <- (1:n) / n
  SSE <- function(theta, y, t) {
    X_theta <- cbind(t * (t <= theta), t * (t > theta))
    reg_fit  <- lm.fit(X_theta, y)
    sum(reg_fit$residuals^2)
  }
  result <- apply(epsilon, MARGIN = 2, FUN = function(error) {
      y <- beta_1 * t * (t <= theta) + beta_2 * t * (t > theta) +
           error
      trial_values <- dqrunif(100)
      theta_start <- trial_values[which.min(sapply(trial_values,
                     SSE, t = t, y = y))]
      theta_hat <- optim(theta_start, SSE, y = y, t = t,
                   method = "Brent", lower = 0, upper = 1)$par
      X_hat <- cbind(t*(t <= theta_hat), t*(t > theta_hat))
      reg_fit <- lm.fit(X_hat, y)
      e   <- reg_fit$residuals
      fit <- reg_fit$fitted.values
      theta_hat_star <- numeric(B)
      for (b in seq_len(B)) {
        y_star <- fit + dqsample(e, n, replace = TRUE)
        trial_values <- dqrunif(100)
        theta_start <- trial_values[which.min(sapply(trial_values,
                       SSE, t = t, y = y_star))]
        theta_hat_star[b] <- optim(theta_start, SSE, y = y_star,
                                   t = t, method = "Brent",
                                   lower = 0, upper = 1)$par
      }
      c(theta_hat = theta_hat,
        lower = quantile(theta_hat_star, 0.025, type = 1,
                         names = FALSE),
        upper = quantile(theta_hat_star, 0.975, type = 1,
                         names = FALSE))
    }
  )
  result
}'
  expect_identical(
    sim_template(regression_worker, 1234, 200, 4, 2, .5, .25, 1, 1.5, 20),
    book_template(reg_src, 1234, 200, 4, 2, .5, .25, 1, 1.5, 20)
  )
})
