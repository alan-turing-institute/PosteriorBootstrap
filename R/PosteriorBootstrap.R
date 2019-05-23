#' PosteriorBootstrap: A package with a parallel approach for adaptive
#' non-parametric learning
#'
#' The foo package provides two categories of functions. The first category
#' returns or loads the system files that ship with the package: get_stan_file,
#' get_german_credit_file, get_german_credit_dataset. The second category
#' performs statistical sampling: stick_breaking and anpl (for
#' "adaptive non-parametric learning").
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

k_extdata <- "extdata"
k_package <- "PosteriorBootstrap"
k_german_credit <- "statlog-german-credit.dat"
k_german_credit_url <- paste0("http://archive.ics.uci.edu/ml/",
                              "machine-learning-databases/statlog/",
                              "german/german.data-numeric")
k_rstan_model <- "bayes_logit.stan"

.onAttach <- function(libname, pkgname) {
  msg <- paste0("Welcome to PosteriorBootstrap, a parallel approach for ",
                "adaptive non-parametric learning")
  packageStartupMessage(msg)
}

data_file <- function(name) {
  return(system.file(k_extdata, name, package = k_package))
}

#' Stan file with Bayesian Logistic Regression.
#'
#' @return An RStan file with the model for variational Bayes that ships with
#'   this package (extension \code{.stan}).
#'
#' @examples
#' f <- get_stan_file()
#' \dontrun{
#' file.show(f)
#' }
#'
#' @export
get_stan_file <- function() {
  return(data_file(k_rstan_model))
}

#' File with the German Statlog credit dataset.
#'
#' @return A file with the plain-text raw data for the German Statlog credit
#'   that ships with this package (extension \code{.dat}).
#'
#' @examples
#' f <- get_german_credit_file()
#' \dontrun{
#' file.show(f)
#' }
#'
#' @export
get_german_credit_file <- function() {
  return(data_file(k_german_credit))
}

#' Load and pre-process the dataset that ships with the package.
#'
#' @param scale Whether to scale the features to have mean 0 and variance 1.
#' @param add_intercept Whether to add an intercept as the first feature.
#' @param download_destination Provide a filepath if you want to download the
#'   dataset from source. Note that although the original dataset has 20
#'   features (some of them qualitative), the numeric dataset has 24 features.
#'
#' @return A list with fields \code{x} for features and \code{y} for outcomes.
#'
#' @examples
#' german <- get_german_credit_dataset()
#' head(german$y)
#' head(german$x)
#'
#' @export
get_german_credit_dataset <- function(scale = TRUE, add_intercept = TRUE,
                                      download_destination = NULL) {

  if (is.null(download_destination)) {
    filepath <- get_german_credit_file()
  } else {
    utils::download.file(k_german_credit_url, download_destination)
    filepath <- download_destination
  }
  raw_dataset <- as.matrix(utils::read.table(filepath))
  colnames(raw_dataset) <- NULL

  x <- raw_dataset[, 1:(ncol(raw_dataset) - 1)]
  y <- raw_dataset[, ncol(raw_dataset)]

  # German statlog dataset outcomes are (1, 2), so
  # subtract 1 here to make them (0, 1)
  y <- y - 1
  stopifnot(all(y %in% c(0, 1)))

  # Standardise features to have mean 0 and variance 1
  if (scale) {
    x <- scale(x)
  }
  if (add_intercept) {
    x <- cbind(1, x)
  }

  # Return the list object
  return(list(x = x, y = y))
}

#' Run variational Bayes.
#'
#' \code{run_variational_bayes} returns samples of the parameters estimated with
#' `rstan` on the data provided in the arguments.
#'
#' @param x The features of the data for the model
#' @param y The outcomes of the data for the model
#' @param output_samples The number of output samples to draw
#' @param beta_sd The standard deviation of the prior on the parameters
#' @param stan_file A custom Stan file, if different from the default which
#'   ships with the package and models Bayesian logistic regression
#' @param iter The maximum number of iterations for Rstan to run
#' @param seed A seed to start the Rstan sampling
#' @param verbose Whether to print output from RStan
#' @return Matrix of size `output_samples`x`ncol(x)` drawn from the RStan
#'   model applied to `y` and `x`
#'
#' @importFrom Rcpp cpp_object_initializer
#' @export
run_variational_bayes <- function(x, y, output_samples, beta_sd,
                                  stan_file = get_stan_file(),
                                  iter = 10000, seed = 123, verbose = FALSE) {

  # Check inputs
  if (length(y) != nrow(x)) {
    stop("The length of y must be the same as the first dimension of x")
  }
  if (!all(y %in% c(0, 1))) {
    stop("The values of y must be in (0, 1)")
  }

  n_input <- length(y)
  p <- ncol(x)

  train_dat <- list(n = n_input, p = p, x = x, y = y, beta_sd = beta_sd)

  stan_model <- rstan::stan_model(file = stan_file)

  params <- list(object = stan_model, data = train_dat, seed = seed,
                 output_samples = output_samples, iter = iter)
  if (verbose) {
    stan_vb <- do.call(rstan::vb, params)
  } else {
    log <- utils::capture.output(
      stan_vb <- do.call(rstan::vb, params)
    )
  }

  return(rstan::extract(stan_vb)$beta)
}

#' Stick-breaking depending on a concentration parameter.
#'
#' \code{stick_breaking} returns a vector with the breaks of a stick of length 1
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
#' @return A vector of stick-breaks summing to one.
#' @examples
#' stick_breaking(1)
#' stick_breaking(1, min_stick_breaks = 10)
#' stick_breaking(1, min_stick_breaks = 10, threshold = 1e-8)
#'
#' @export
stick_breaking <- function(concentration = 1,
                           min_stick_breaks = 100,
                           threshold = 1e-8) {

  # This algorithm regenerates all values, which is a waste but is faster than
  # appending in a for loop, which is slow in R.  The loop is often unnecessary,
  # as the function is called with min_stick_breaks equal to the the number of
  # rows in the dataset, which is 1,000, and it only goes once through the
  # loop. This algorithm also uses some shortcuts in array multiplication to get
  # the right numbers.

  # The array `stick_remaining` holds the remainder of the stick if we stopped
  # at that index: 1, 1-beta_draws[1], (1-beta_draws[1])*(1-beta_draws[2]), ...
  # The algorithm is statistically guaranteed to finish because the last value
  # of the `stick_remaining` array is a product of numbers between 0 and 1, so
  # increasing the number of values in that product drives it to zero (even if
  # we draw new values each time).

  num_stick_breaks <- min_stick_breaks
  while (TRUE) {
    # Draw `num_stick_breaks` independent numbers from the
    # Beta(1, concentration) distribution. All numbers are between 0 and 1.
    beta_draws <- stats::rbeta(n = num_stick_breaks,
                               shape1 = 1, shape2 = concentration)

    # Calculate stick remaining at each index.
    stick_remaining <- c(1, cumprod(1 - beta_draws))

    if (stick_remaining[length(stick_remaining)] <= threshold) {
      break
    } else {
      # Increase the number of stick breaks to go below the threshold.
      # The magic number 10 here is arbitrary.
      num_stick_breaks <- num_stick_breaks * 10
    }
  }

  # The stick breaks are the beta_draws multiplied by the stick remaining,
  # which starts at 1.
  stick_breaks <- stick_remaining[1:num_stick_breaks] * beta_draws

  # Divide by the sum to remove the unallocated stick due to the threshold
  # and so it adds up to one
  stick_breaks <- stick_breaks / sum(stick_breaks)

  return(stick_breaks)
}

check_inputs <- function(x, y, concentration, n_bootstrap, posterior_sample,
                         gamma_mean, gamma_vcov) {
  if (is.null(posterior_sample)) {
    if (is.null(gamma_mean) | is.null(gamma_vcov)) {
      stop(paste0("If you don't provide a posterior sample, ",
                  "you must provide a centering model"))
    }

    if (! all(is.numeric(gamma_mean)) & all(is.numeric(gamma_vcov))) {
      stop(paste0("Invalid input: the mean and variance-covariance ",
                  "of the centering model need to be numeric"))
    }

    if (!(is.numeric(gamma_mean) & is.numeric(gamma_vcov))) {
      stop(paste0("The mean and variance-covariance of the centering model ",
                  "have to be numeric."))
    }

    dim_gamma <- ncol(x)
    if (!(dim_gamma == length(gamma_mean))) {
      stop(paste0("You need to give a vector for the mean of the centering ",
                  "model with size ",
                  dim_gamma,
                  " (for the number of covariates) and instead you gave ",
                  length(gamma_mean),
                  "."))
    }
    if (!all(c(dim_gamma, dim_gamma) == dim(gamma_vcov))) {
      stop(paste0("You need to give a matrix for the variance-covariance ",
                  "matrix of the centering model: ",
                  dim_gamma, "*", dim_gamma,
                  " (for the number of covariates) and instead you gave ",
                  dim(gamma_vcov),
                  "."))
    }
  }

  if (!is.null(posterior_sample)) {
    if (ncol(posterior_sample) != ncol(x)) {
      stop(paste0("The number of columns in the posterior sample must be the ",
                  "same as the number of covariates"))
    }
    if (nrow(posterior_sample) < n_bootstrap) {
      stop(paste0("The posterior sample must have a number of rows no smaller ",
                  "than n_bootstrap"))
    }
  }

  if (!is.numeric(concentration)) {
    stop("Concentration needs to be numeric")
  }
  if (concentration < 0) {
    stop("Concentration needs to be positive")
  }
  if (!all(y %in% c(0, 1))) {
    stop("The values of y must all be in (0, 1)")
  }
}

#' Adaptive non-parametric learning function.
#'
#' \code{anpl} returns samples of the parameter of interest
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
#'   `n_bootrstap` (as the algorithm draws a new sample based on a single draw
#'   of the posterior sample).
#' @param gamma_mean In case \code{posterior_sample} is NULL, the mean for the
#'   centering model (equation 9, page 4)
#' @param gamma_vcov In case \code{posterior_sample} is NULL, the
#'   variance-covariance of the centering model for gamma (equation 9, page 4).
#' @param threshold The threshold of stick remaining below which the function
#'   stops looking for more stick-breaks. It correspondes to epsilon in the
#'   paper, at the bottom of page 5 and in algorithm 2 in page 12.
#' @param num_cores Number of processor cores for the parallel run of the
#'   algorithm. See \code{mc.cores} in \link[parallel]{mclapply} for details.
#' @param show_progress Boolean whether to show the progress of the algorithm in
#'   a progress bar.
#' @return A matrix of bootstrap samples for the parameter of interest
#'
#' @export
anpl <- function(x,
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
    progress_bar <- utils::txtProgressBar(min = 0, max = n_bootstrap, style = 3)
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
  theta_transpose <- parallel::mcmapply(anpl_single, 1:n_bootstrap,
                                        MoreArgs = more_args, mc.cores = num_cores)

  # Verify dimensions
  stopifnot(all(dim(theta_transpose) == c(ncol(x), n_bootstrap)))

  return(t(theta_transpose))
}

anpl_single <- function(i, x, y, concentration, gamma, threshold,
                        progress_bar) {

  dataset_n <- length(y)
  if (0 == concentration) {
    # No prior samples. Compute Dirichlet weights
    wgt_mean <- rep(1, dataset_n)
    wgts <- stats::rgamma(n = dataset_n,
                          shape = wgt_mean,
                          rate = rep(1, dataset_n))
    x_all <- x
    y_all <- y
  } else {
    gamma_i  <- gamma[i, ]

    # Get stick-breaking split between data and model
    # s_i is random around c / (c + n)
    # s_i is s^{(i)}, the "model vs data weight" in algorithm 2, page 12
    s_i <- stats::rbeta(n = 1,
                        shape1 = concentration,
                        shape2 = dataset_n)

    # Generate stick-breaking weights. The number of weights,
    # `n_centering_model_samples` is `10^k * dataset_n` for integer k because
    # the stick_breaking function is written like that at the moment.

    w_raw_model <- stick_breaking(concentration = concentration,
                                  min_stick_breaks = dataset_n,
                                  threshold = threshold)
    w_model <- w_raw_model * s_i
    n_centering_model_samples <- length(w_model)

    # Note: the stick-breaking function serves only to set
    # n_centering_model_samples, which is probably wrong. See issue 59:
    # https://github.com/alan-turing-institute/PosteriorBootstrap/issues/59

    # Create prior samples (prior in the code means "model" in the
    # paper). `x_prior` is a `n_centering_model_samples * ncol(x)` matrix and
    # contains `x` vertically stacked to reach `n_centering_model_samples`
    # rows (because the length of the output of stick_breaking() is a multiple
    # of dataset_n).

    x_prior <- kronecker(rep(1, n_centering_model_samples / dataset_n), x)
    stopifnot(all(c(n_centering_model_samples, ncol(x)) == dim(x_prior)))

    # Generate classes from features, see page 7 of the paper: "When
    # generating synthetic samples for the posterior bootstrap, both features
    # and classes are needed. Classes are generated, given features, according
    # to the probability specified by the logistic distribution."  e1071 is a
    # package that provides such a distribution in the sigmoid() function
    probs <- e1071::sigmoid(x_prior %*% gamma_i)
    stopifnot(c(1, n_centering_model_samples) == dim(probs))
    y_prior <- stats::rbinom(n = n_centering_model_samples,
                             size = 1, prob = probs)

    # Compute Dirichlet weights for the data and the model
    wgt_mean <- c(rep(1, dataset_n),
                  rep(concentration / n_centering_model_samples,
                      n_centering_model_samples))
    n_total <- dataset_n + n_centering_model_samples
    wgts <- stats::rgamma(n = n_total, shape = wgt_mean,
                          rate = rep(1, n_total))
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
