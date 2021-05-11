# This file contains the package documentation and the main functions of the
# package: a welcome message, exported functions (`draw_stick_breaks` and
# `draw_logit_samples`), and internal functions (`check_inputs` and and
# `draw_logit_single`)

# Package documentation --------------------------------------------------------

#' A package with a parallel approach for adaptive non-parametric learning
#'
#' The PosteriorBootstrap package provides two categories of functions.
#' The first category returns or loads the system files that ship with the
#' package: get_stan_file, get_german_credit_file, get_german_credit_dataset.
#' The second category performs statistical sampling: draw_stick_breaks and
#' draw_logit_samples (for adaptive non-parametric learning of the logistic
#' regression model).
#'
#' Please see the vignette for sample usage and performance metrics.
#'
#' @docType package
#' @name PosteriorBootstrap
NULL

requireNamespace("e1071", quietly = TRUE)
requireNamespace("MASS", quietly = TRUE)
requireNamespace("parallel", quietly = TRUE)
requireNamespace("rstan", quietly = TRUE)
requireNamespace("stats", quietly = TRUE)
requireNamespace("utils", quietly = TRUE)

.onAttach <- function(libname, pkgname) {  # nolint
  msg <- paste0("Welcome to PosteriorBootstrap, a parallel approach for ",
                "adaptive non-parametric learning")
  packageStartupMessage(msg)
}

# Stick-breaking function ------------------------------------------------------

#' Draw stick-breaks depending on a concentration parameter
#'
#' \code{draw_stick_breaks} returns a vector with the breaks of a stick of
#' length 1.
#'
#' This function implements the stick-breaking process for non-parametric
#' learning described in section 2 of the supplementary material. The name
#' "stick-breaking" comes from a stick of unit length that we need to break into
#' a number of items. This code implements algorithm 2 and the stick-breaking
#' function calculates the parameter T in algorithm 1, which is the only
#' difference between the two algorithms. The code uses the Beta distribution as
#' that distribution is part of the definition of the stick-breaking process.
#' The function draws from the beta distribution, e.g. \code{b_1}, \code{b_2},
#' \code{b_3}, ..., and computes the stick breaks as \code{b_1},
#' \code{(1-b_1)*b_2}, \code{(1-b_1)*(1-b_2)*b_3}, ... . The length remaining in
#' the stick at each step is \code{1-b_1}, \code{(1-b_1)* (1-b_2)},
#' \code{(1-b_1)*(1-b_2)*(1-b_3)}, ... so the latter converges to zero.
#'
#' @param concentration The parameter \code{c} in the paper (page 3, formula 3),
#'   which is an effective sample size.
#' @param min_stick_breaks The minimal number of stick-breaks.
#' @param threshold The threshold of stick remaining below which the function
#'   stops looking for more stick-breaks. It corresponds to epsilon in the
#'   paper, at the bottom of page 5 and in algorithm 2 in page 12.
#' @param seed A seed to start the sampling.
#' @return A vector of stick-breaks summing to one.
#' @examples
#' draw_stick_breaks(1)
#' draw_stick_breaks(1, min_stick_breaks = 10)
#' draw_stick_breaks(1, min_stick_breaks = 10, threshold = 1e-8)
#'
#' @export
draw_stick_breaks <- function(concentration = 1,
                              min_stick_breaks = 100,
                              threshold = 1e-8,
                              seed = NULL) {

  # This algorithm generates a set of stick-breaks and then checks whether the
  # remainder is under the required threshold. If it is, then the stick breaks
  # are returned, otherwise the number of breaks is increased and a new set of
  # breaks is generated. Although it may seem wasteful to regenerate the values,
  # this is nevertheless faster than appending values to an existing array.

  # The array `stick_remaining` holds the remainder of the stick if we stopped
  # at that index: 1, 1-beta_draws[1], (1-beta_draws[1])*(1-beta_draws[2]), ...
  # The algorithm is statistically guaranteed to finish because the last value
  # of the `stick_remaining` array is a product of numbers between 0 and 1, so
  # increasing the number of values in that product drives it to zero (even if
  # we draw new values each time).

  # Set the seed, if any
  if (!is.null(seed)) {
    set.seed(seed)
  }

  num_stick_breaks <- min_stick_breaks
  while (TRUE) {
    # Draw `num_stick_breaks` independent numbers from the
    # Beta(1, concentration) distribution. All numbers are between 0 and 1.
    beta_draws <- stats::rbeta(n = num_stick_breaks,
                               shape1 = 1, shape2 = concentration)

    # Calculate stick remaining at each index, starting at 1. The result is a
    # vector of size `num_stick_breaks + 1` with the running cumulative product,
    # i.e. the amount stick remaining at each index.
    stick_remaining <- c(1, cumprod(1 - beta_draws))

    if (stick_remaining[length(stick_remaining)] <= threshold) {
      break
    } else {
      # Increase the number of stick breaks to go below the threshold.  The
      # magic number 2 here is arbitrary. A larger number can speed up the
      # running time of this loop but increases the number of samples in the
      # estimation and can thus increase the total running time of the code.
      num_stick_breaks <- num_stick_breaks * 2
    }
  }

  # The stick breaks are the difference from one value of stick_remaining to the
  # next
  stick_breaks <- stick_remaining[1:num_stick_breaks] -
    stick_remaining[2:(num_stick_breaks + 1)]

  # Divide by the sum to remove the unallocated stick due to the threshold
  # and so it adds up to one
  stick_breaks <- stick_breaks / sum(stick_breaks)

  return(stick_breaks)
}

# Input checking for draw_logit_samples ----------------------------------------

check_inputs <- function(x, y, concentration, n_bootstrap, posterior_sample,
                         gamma_mean, gamma_vcov) {

  # The error messages here are directly copied from test_anpl.R
  if (is.null(posterior_sample)) {
    if (is.null(gamma_mean)) {
      stop(paste0("If you don't provide a posterior sample, you ",
                  "must provide a mean for the centering model"))
    }
    if (is.null(gamma_vcov)) {
      stop(paste0("If you don't provide a posterior sample, you ",
                               "must provide a variance-covariance for the ",
                               "centering model"))
      }

    if (!(all(is.numeric(gamma_mean)) & all(is.numeric(gamma_vcov)))) {
      stop(paste0("Invalid input: the mean and variance-",
                  "covariance of the centering model need ",
                  "to be numeric"))
    }

    dim_gamma <- ncol(x)
    if (!(dim_gamma == length(gamma_mean))) {
      stop(paste0("You need to give a vector for the mean ",
                  "of the centering model with size ",
                  dim_gamma,
                  " (for the number of covariates) and instead you gave ",
                  length(gamma_mean),
                  "."))
    }
    if (!all(c(dim_gamma, dim_gamma) == dim(gamma_vcov))) {
      stop(paste0("You need to give a matrix for the variance-",
                  "covariance matrix of the centering model: ",
                  dim_gamma, "*", dim_gamma,
                  " (for the number of covariates) and instead you gave ",
                  dim(gamma_vcov),
                  "."))
    }
  }

  if (!is.null(posterior_sample)) {
    if (ncol(posterior_sample) != ncol(x)) {
      stop(paste0("The number of columns in the posterior sample ",
                  "must be the same as the number of covariates"))
    }
    if (nrow(posterior_sample) < n_bootstrap) {
      stop(paste0("The posterior sample must have a number of ",
                  "rows no smaller than n_bootstrap"))
    }
  }

  if (!is.numeric(concentration)) {
    stop("Concentration needs to be numeric")
  }
  if (concentration < 0) {
    stop("Concentration needs to be positive or zero")
  }
  if (!all(y %in% c(0, 1))) {
    stop("The values of y must all be in (0, 1)")
  }
}

# Draw logit samples -----------------------------------------------------------

#' Draw adaptive non-parametric learning samples for logistic regression
#'
#' \code{draw_logit_samples} returns samples of the parameter of interest in a
#' logistic regression.
#'
#' This function implements the non-parametric-learning algorithm, which is
#' algorithm 2 in page 12 in the paper. It uses a mixture of Dirichlet processes
#' and stick-breaking to find the number of posterior samples and logistic
#' regression to find the randomized parameter of interest. For examples, see
#' the vignette.
#'
#' @param x The features of the data.
#' @param y The outcomes of the data (either \code{0} or \code{1}).
#' @param concentration The parameter \code{c} in the paper (page 3, formula 3),
#' @param n_bootstrap The number of bootstrap samples required.
#' @param posterior_sample The function can take samples from the posterior to
#'   generate non-parametric-learning samples, or it can take NULL and the
#'   posterior is assumed normal N(\code{gamma_mean}, \code{gamma_vcov}). If
#'   provided, the posterior sample must have a number of columns equal to the
#'   number of covariates and a number of rows equal or larger than the
#'   `n_bootstrap` (as the algorithm draws a new sample based on a single draw
#'   of the posterior sample).
#' @param gamma_mean In case \code{posterior_sample} is NULL, the mean for the
#'   centering model (equation 9, page 4).
#' @param gamma_vcov In case \code{posterior_sample} is NULL, the
#'   variance-covariance of the centering model for gamma (equation 9, page 4).
#' @param threshold The threshold of stick remaining below which the function
#'   stops looking for more stick-breaks. It correspondes to epsilon in the
#'   paper, at the bottom of page 5 and in algorithm 2 in page 12.
#' @param num_cores Number of processor cores for the parallel run of the
#'   algorithm. See \code{mc.cores} in \link[parallel]{mclapply} for details.
#' @param show_progress Boolean whether to show the progress of the algorithm in
#'   a progress bar.
#' @return A matrix of bootstrap samples for the parameter of interest.
#'
#' @export
draw_logit_samples <- function(x,
                               y,
                               concentration,
                               n_bootstrap = 100,
                               posterior_sample = NULL,
                               gamma_mean = NULL,
                               gamma_vcov = NULL,
                               threshold = 1e-8,
                               num_cores = 1,
                               show_progress = FALSE) {

  # Check inputs
  check_inputs(x = x, y = y, concentration = concentration,
               n_bootstrap = n_bootstrap, posterior_sample = posterior_sample,
               gamma_mean = gamma_mean, gamma_vcov = gamma_vcov)

  # Get mixing theta
  if (is.null(posterior_sample)) {
    gamma <- MASS::mvrnorm(n = n_bootstrap, mu = gamma_mean, Sigma = gamma_vcov)
  } else {
    gamma <- posterior_sample
  }

  if (show_progress) {
    # This progress bar serves to keep the Travis build alive.
    progress_bar <- utils::txtProgressBar(min = 0, max = n_bootstrap, style = 1)
  } else {
    progress_bar <- NULL
  }

  # The parallel `mcmapply` requires the constant arguments to go as a list in
  # `MoreArgs`
  more_args <- list("x" = x,
                    "y" = y,
                    "concentration" = concentration,
                    "gamma" = gamma,
                    "threshold" = threshold,
                    "progress_bar" = progress_bar)

  # Generate samples in parallel and transpose the result. `mcmapply` returns a
  # matrix where the result is flipped: an additional run goes into a new column
  # and not a new row.
  theta_transpose <- parallel::mcmapply(draw_logit_single, 1:n_bootstrap,
                                        MoreArgs = more_args,
                                        mc.cores = num_cores)

  # Verify dimensions
  stopifnot(all(dim(theta_transpose) == c(ncol(x), n_bootstrap)))

  return(t(theta_transpose))
}

# Parallelised draw of a logit sample ------------------------------------------

draw_logit_single <- function(i, x, y, concentration, gamma, threshold,
                              progress_bar) {

  dataset_n <- length(y)

  # Compute Dirichlet weights for the data using Gamma draws, see
  # https://en.wikipedia.org/wiki/Dirichlet_distribution#Gamma_distribution
  w_raw_data <- stats::rgamma(n = dataset_n,
                              shape = rep(1, dataset_n),
                              rate = rep(1, dataset_n))
  w_data <- w_raw_data / sum(w_raw_data)

  if (0 == concentration) {
    # In the paper, the model versus data weight (s_i) is Beta(concentration,
    # dataset_n). In the case concentration = 0, the Beta distribution tends to
    # a degenerate distribution at 0, which corresponds to no weight on the
    # model, which means that this sample draw uses only the data.

    wgts <- w_data
    x_all <- x
    y_all <- y
  } else {
    gamma_i  <- gamma[i, ]

    # Draw split between data and model
    # s_i is random around c / (c + n)
    # s_i is s^{(i)}, the "model vs data weight" in algorithm 2, page 12
    s_i <- stats::rbeta(n = 1,
                        shape1 = concentration,
                        shape2 = dataset_n)

    # Generate stick-breaking weights. The number of weights,
    # `n_centering_model_samples` is `2^k * dataset_n` for integer k because
    # the draw_stick_breaks function is written like that at the moment.
    #
    # The threshold is not exactly as in the paper but it matters little since
    # the `draw_stick_breaks` function increases size by factors of 2 so it
    # doesn't stop when the threshold is exactly exceeded anyway .

    w_model <- draw_stick_breaks(concentration = concentration,
                                 min_stick_breaks = dataset_n,
                                 threshold = threshold)
    n_centering_model_samples <- length(w_model)

    wgts <- c(w_data * (1 - s_i), w_model * s_i)

    # Create prior samples (prior in the code means "model" in the
    # paper). `x_prior` is a `n_centering_model_samples * ncol(x)` matrix and
    # contains `x` vertically stacked to reach `n_centering_model_samples` rows
    # (because the length of the output of draw_stick_breaks() is a multiple of
    # dataset_n).

    # Note that this Kronecker product is well-defined because the minimum
    # number of stick breaks from `draw_stick_breaks()` is `dataset_n`, so the
    # left matrix is at least `diag(1)`.

    x_prior <- kronecker(rep(1, n_centering_model_samples / dataset_n), x)
    stopifnot(all(c(n_centering_model_samples, ncol(x)) == dim(x_prior)))

    # Generate classes from features, see page 7 of the paper: "When
    # generating synthetic samples for the posterior bootstrap, both features
    # and classes are needed. Classes are generated, given features, according
    # to the probability specified by the logistic distribution."  e1071 is a
    # package that provides such a distribution in the sigmoid() function
    probs <- e1071::sigmoid(x_prior %*% gamma_i)
    stopifnot(all(c(n_centering_model_samples, 1) == dim(probs)))
    y_prior <- stats::rbinom(n = n_centering_model_samples,
                             size = 1, prob = probs)

    x_all <- rbind(x, x_prior)
    y_all <- c(y, y_prior)
  }
  stopifnot(all(y_all %in% c(0, 1)))

  # Parameter is computed via weighted glm fit.  We use quasibinomial family
  # instead of binomial to allow the count parameters to be
  # non-integers. Quasibinomial is the same as binomial except that it removes
  # the integer check and does not compute AIC. See the answer by the
  # author (mmorin) on this StackOverflow thread for more details:
  # https://stackoverflow.com/questions/12953045
  glm_fit <- stats::glm.fit(x = x_all,
                            y = y_all,
                            weights = wgts,
                            family = stats::quasibinomial(link = "logit"))

  if (!is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, i)
  }

  return(glm_fit$coefficients)
}
