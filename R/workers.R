#' Worker: estimate pi by dart throwing
#'
#' The Chapter 3 example. Each replication throws `n` uniform points at the
#' square \eqn{[-1, 1]^2} and estimates \eqn{\pi} as four times the fraction
#' that land inside the unit circle.
#'
#' @inheritParams sim_template
#' @param set_up One element of [make_set_up()].
#' @param ... Ignored.
#'
#' @return A numeric vector of `set_up$n_reps` estimates of \eqn{\pi}.
#' @export
#' @family workers
#'
#' @examples
#' out <- sim_template(pi_worker, 1234, 100, 1000, 2)
#' mean(bind_results(out))
pi_worker <- function(n, set_up, ...) {
  # unpack the parameters
  n_reps <- set_up$n_reps

  # set the seed and stream
  set_worker_rng(set_up)

  # generate matrices with replications in columns
  x <- matrix(
    dqrng::dqrunif(n * n_reps, min = -1, max = 1), # uniform on [-1, 1]
    nrow = n,
    ncol = n_reps
  )

  y <- matrix(
    dqrng::dqrunif(n * n_reps, min = -1, max = 1), # uniform on [-1, 1]
    nrow = n,
    ncol = n_reps
  )

  # estimate pi for each replication
  pi_hat <- 4 * colMeans(x^2 + y^2 <= 1)

  pi_hat
}

#' Worker: Cauchy scale via the interquartile range
#'
#' The Chapter 3 Cauchy example. Each replication draws `n` observations from
#' a Cauchy distribution with scale `theta` by the quantile transformation and
#' estimates the scale as half the sample interquartile range.
#'
#' @inheritParams pi_worker
#' @param ... The scale parameter `theta` as the first element.
#'
#' @return A numeric vector of `set_up$n_reps` scale estimates.
#' @export
#' @family workers
#'
#' @examples
#' out <- sim_template(iqr_worker, 1234, 100, 20, 2, 5)
#' lapply(out, round, 3)
iqr_worker <- function(n, set_up, ...) {
  # unbundle the simulation parameters
  n_reps <- set_up$n_reps

  # unpack the value of the scale parameter
  param_list <- list(...)
  theta <- param_list[[1]]

  # set the seed and increment
  set_worker_rng(set_up)

  # array of random uniforms
  U <- matrix(dqrng::dqrunif(n * n_reps), nrow = n, ncol = n_reps)

  # quantile transformation
  X <- theta * tan(pi * (U - .5))

  # columnwise quartiles
  Q <- matrixStats::colQuantiles(X, probs = c(0.25, 0.75), type = 7)

  # scale estimates
  theta_hat <- 0.5 * (Q[, 2] - Q[, 1])

  theta_hat
}

#' Worker: segmented regression bootstrap
#'
#' The Chapter 5 case study. Each replication generates data from the
#' change-point model
#' \eqn{y = \beta_1 t\, 1(t \le \theta) + \beta_2 t\, 1(t > \theta) + \epsilon}
#' on the grid \eqn{t = 1/n, \ldots, 1} with normal errors of standard
#' deviation `sigma`, estimates the change point \eqn{\theta} by least
#' squares (a 100-point random search for a starting value followed by
#' Brent's method), and forms a residual-bootstrap percentile interval from
#' `B` resamples.
#'
#' @inheritParams pi_worker
#' @param ... The model parameters, in order: `theta`, `sigma`, `beta_1`,
#'   `beta_2`, and `B` (the number of bootstrap resamples).
#'
#' @return A `3 x set_up$n_reps` matrix with rows `theta_hat`, `lower`, and
#'   `upper`.
#' @export
#' @family workers
#'
#' @examples
#' \donttest{
#' out <- sim_template(regression_worker, 1234, 200, 4, 2, .5, .25, 1, 1.5, 50)
#' bind_results(out)
#' }
regression_worker <- function(n, set_up, ...) {
  # unbundle the simulation parameters
  n_reps <- set_up$n_reps

  # set the seed and stream
  set_worker_rng(set_up)

  # unpack the model parameters
  param_list <- list(...)
  theta  <- param_list[[1]]
  sigma  <- param_list[[2]]
  beta_1 <- param_list[[3]]
  beta_2 <- param_list[[4]]
  B      <- param_list[[5]]

  # generate normal errors with replications in columns
  epsilon <- matrix(dqrng::dqrnorm(n * n_reps, mean = 0, sd = sigma),
                    nrow = n, ncol = n_reps)

  # independent variable
  t <- (1:n) / n

  # error sum-of-squares function
  SSE <- function(theta, y, t) {
    X_theta <- cbind(t * (t <= theta), t * (t > theta))
    reg_fit <- stats::lm.fit(X_theta, y)
    sum(reg_fit$residuals^2)
  }

  # carry out the simulation one replication at a time
  result <- apply(epsilon, MARGIN = 2, FUN = function(error) {
    # create the response data
    y <- beta_1 * t * (t <= theta) + beta_2 * t * (t > theta) +
      error

    # search for a starting value
    trial_values <- dqrng::dqrunif(100)
    theta_start <- trial_values[which.min(sapply(trial_values,
                                                 SSE, t = t, y = y))]

    # estimate the breakpoint
    theta_hat <- stats::optim(theta_start, SSE, y = y, t = t,
                              method = "Brent", lower = 0, upper = 1)$par

    # obtain residuals and fitted values
    X_hat <- cbind(t * (t <= theta_hat), t * (t > theta_hat))

    reg_fit <- stats::lm.fit(X_hat, y)

    e   <- reg_fit$residuals
    fit <- reg_fit$fitted.values

    # collect the bootstrap estimates
    theta_hat_star <- numeric(B)

    for (b in seq_len(B)) {
      # construct bootstrap data
      y_star <- fit + dqrng::dqsample(e, n, replace = TRUE)

      # search for a starting value
      trial_values <- dqrng::dqrunif(100)
      theta_start <- trial_values[which.min(sapply(trial_values,
                                                   SSE, t = t, y = y_star))]

      # bootstrap estimate
      theta_hat_star[b] <- stats::optim(theta_start, SSE, y = y_star,
                                        t = t, method = "Brent",
                                        lower = 0, upper = 1)$par
    }

    # estimator and bootstrap confidence interval
    c(theta_hat = theta_hat,
      lower = stats::quantile(theta_hat_star, 0.025, type = 1,
                              names = FALSE),
      upper = stats::quantile(theta_hat_star, 0.975, type = 1,
                              names = FALSE))
  })

  result
}
