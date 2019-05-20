## Code generating figure 2 in Lyddon, Walker & Holmes, 2018.
options(warn = 1)

requireNamespace("BayesLogit", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)
requireNamespace("e1071", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("MASS", quietly = TRUE)
requireNamespace("parallel", quietly = TRUE)
requireNamespace("PolyaGamma", quietly = TRUE)
requireNamespace("rstan", quietly = TRUE)
requireNamespace("stats", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("utils", quietly = TRUE)

k_extdata <- "extdata"
k_package <- "PosteriorBootstrap"
k_german_credit <- "statlog-german-credit.dat"
k_rstan_model <- "bayes_logit.stan"

data_file <- function(name) {
  return(system.file(k_extdata, name, package = k_package))
}

#' Get the RStan file with Variational Bayes for comparison
#'
#' @return An RStan file with the model for variational Bayes that ships with
#'   this package (extension \code{.stan}).
#' @export
get_rstan_file <- function() {
  return(data_file(k_rstan_model))
}

get_german_credit_file <- function() {
  return(data_file(k_german_credit))
}

#' Function to load dataset. Won't be necessary in the package
#'
#' @param dataset_input_list A list with a name and, in the case of "toy", a
#'   number of samples
#'
#' @return A dataset in the right format
#' @export
load_dataset <- function(dataset_input_list = list(name = "toy", n = 200)) {
  # Check dataset_input_list has required elements
  if (!(c("name") %in% names(dataset_input_list))) {
    stop("dataset_input_list does not contain name")
  }

  # dataset object is list we will return.
  dataset <- list(name = dataset_input_list$name)

  if (dataset$name == "toy") {
    if (!("n" %in% names(dataset_input_list))) {
      stop("dataset_input_list does not contain n")
    }

    # Generate the dataset

    # This line gives values 0 or 1
    raw_dataset <- stats::rbinom(n = dataset_input_list$n,
                                 size = 1,
                                 prob = 0.5)
    raw_dataset <- cbind(stats::rnorm(n = dataset_input_list$n,
                                      mean = raw_dataset * 2 - 1,
                                      sd = 1),
                         raw_dataset)
  } else {
    # Load the dataset
    filepath <- data_file(dataset$name)
    raw_dataset <- as.matrix(utils::read.table(filepath))
    # German statlog dataset outcomes are (1, 2), so
    # subtract 1 here to make them 0, 1, as in the toy dataset
    if (k_german_credit == dataset_input_list$name) {
      raw_dataset[, ncol(raw_dataset)] <- raw_dataset[, ncol(raw_dataset)] - 1
    }

    # Standardise raw dataset to have mean 0 and variance 1, except for the last
    # column (the response)
    col_range <- 1:(ncol(raw_dataset) - 1)
    raw_dataset[, col_range] <- scale(raw_dataset[, col_range])
    stopifnot(all(raw_dataset[, ncol(raw_dataset)] %in% c(0, 1)))

    colnames(raw_dataset) <- NULL
  }

  # Add the dataset to the dataset output list.
  dataset$n <- dim(raw_dataset)[1]
  dataset$n_cov <- dim(raw_dataset)[2] - 1  # Doesn't include an intercept
  dataset$x <- raw_dataset[, 1:dataset$n_cov, drop = FALSE]

  # Convert y to +/- 1 format
  dataset$y <- 2 * raw_dataset[, dataset$n_cov + 1] - 1

  # Return the dataset object.
  return(dataset)
}

# TODO: robustify -1 and 1, or change the Bayes Logit call

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

  # This algorithm regenerates all values, which is a waste
  # but is faster than appending in a for loop, which is slow in R

  # TODO(mmorin): fix this to work around cases where n is not a multiple of 10
  num_stick_breaks <- min_stick_breaks / 10
  stick_remaining <- 1
  while (stick_remaining > threshold) {
    num_stick_breaks <- num_stick_breaks * 10
    u_s <- stats::rbeta(n = num_stick_breaks,
                        shape1 = 1, shape2 = concentration)
    c_rem <- c(1, cumprod(1 - u_s))
    # TODO(mmorin): How is the loop guaranteed to finish if this
    # stick_remaining is a brand new value at each iteration?
    stick_remaining <- c_rem[num_stick_breaks + 1]
  }
  stick_breaks <- c_rem[1:num_stick_breaks] * u_s
  stick_breaks <- stick_breaks / sum(stick_breaks)
  return(stick_breaks)
}

 
#' Adaptive non-parametric learning function
#'
#' \code{anpl} returns samples of the parameter of interest
#'
#' This function implements the non-parametric-learning algorithm, which is
#' algorithm 2 in page 12 in the paper. It uses a mixture of Dirichlet processes
#' and stick-breaking to find the number of posterior samples and logistic
#' regression to find the randomized parameter of interest. For examples, see
#' the vignette.
#'
#' @param dataset The dataset with classes and features (in a specific format
#'   that will be changed)
#' @param concentration The parameter \code{c} in the paper (page 3, formula 3),
#' @param n_bootstrap The number of bootstrap samples required.
#' @param posterior_sample The function can take samples from the posterior to
#'   generate non-parametric-learning samples. Or it can take NULL and the
#'   posterior is assumed normal N(\code{gamma_mean}, \code{gamma_vcov}).
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
anpl <- function(dataset,
                 concentration,
                 n_bootstrap = 100,
                 posterior_sample = NULL,
                 gamma_mean = NULL,
                 gamma_vcov = NULL,
                 threshold = 1e-8,
                 num_cores = 1,
                 show_progress = FALSE) {

  if (is.null(posterior_sample)) {
    if (is.null(gamma_mean) | is.null(gamma_vcov)) {
      stop("You must provide either a posterior sample or a centering model")
    }

    if (! all(is.numeric(gamma_mean)) & all(is.numeric(gamma_vcov))) {
      msg <- paste0("Invalid input: the mean and variance-covariance ",
                    "of the centering model need to be numeric")
      stop(msg)
    }
  }

  if (!is.numeric(concentration)) {
    stop("Concentration needs to be numeric")
  }
  if (concentration < 0) {
    stop("Concentration needs to be positive")
  }
  if (is.null(posterior_sample)) {
    dim_gamma <- dataset$n_cov + 1
    if (!(is.numeric(gamma_mean) & (dim_gamma == length(gamma_mean)))) {
      stop(paste0("You need to give a vector for the mean of the centering ",
                  "model with size ",
                  dim_gamma,
                  " (for the number of covariates, plus 1 for the intercept)"))
    }
    if (!(is.numeric(gamma_vcov) &
            all(c(dim_gamma, dim_gamma) == dim(gamma_vcov)))) {
      stop(paste0("You need to give a matrix for the variance-covariance ",
                  "matrix of the centering model: ",
                  dim_gamma, "*", dim_gamma,
                  " (for the number of covariates, plus 1 for the intercept)"))
    }
  }

  # Get mixing theta
  if (is.null(posterior_sample)) {
    gamma <- MASS::mvrnorm(n = n_bootstrap,
                           mu = gamma_mean,
                           Sigma = gamma_vcov)
  } else {
    gamma <- posterior_sample
  }

  if (show_progress) {
    progress_bar <- utils::txtProgressBar(min = 0, max = n_bootstrap, style = 3)
  } else {
    progress_bar <- NULL
  }

  # The parallel `mcmapply` requires the constant arguments to
  # go as a list in `MoreArgs`
  more_args <- list("dataset" = dataset,
                    "concentration" = concentration,
                    "gamma" = gamma,
                    "threshold" = threshold,
                    "progress_bar" = progress_bar)

  # Generate samples in parallel. `mcmapply` returns a matrix where the result
  # of is flipped, as an additional run goes into a new column and not a new row
  theta_transpose <- parallel::mcmapply(anpl_single, 1:n_bootstrap,
                                     MoreArgs = more_args, mc.cores = num_cores)

  # Verify dimensions
  stopifnot(all(dim(theta_transpose) == c(dataset$n_cov + 1, n_bootstrap)))

  return(t(theta_transpose))
}

anpl_single <- function(i,
                        dataset,
                        concentration,
                        gamma,
                        threshold,
                        progress_bar = NULL) {

    if (concentration != 0) {
      gamma_i  <- gamma[i, ]

      # Get stick-breaking split between data and model
      # s_i is random around c / (c + n)
      # s_i is s^{(i)} is the "model vs data weight" in algorithm 2, page 12
      s_i <- stats::rbeta(n = 1,
                         shape1 = concentration,
                         shape2 = dataset$n)
      # TODO(Simon, MMorin): maybe make s_i deterministic,
      # which only works for algorithm 1, otherwise in algorithm 2
      # the epsilon is just 1/concentration.

      # TODO(mmorin): understand why n_centering_model_samples is 1,000
      # even though concentration is 1
      # TODO(mmorin): understand how vrem = c/(c+n) in the paper
      # maps to this code
      w_raw_model <- stick_breaking(concentration = concentration,
                                    min_stick_breaks = dataset$n,
                                    threshold = threshold)
      w_model <- w_raw_model * s_i
      n_centering_model_samples <- length(w_model)

      # Create prior samples
      # Prior means "model"
      # `x_prior` is a `concentration x n_centering_model_samples` matrix:
      # each row represents a set of prior samples for a particular gamma
      # element.
      # TODO(mmorin): what if n_centering_model_samples is not a multiple
      # of dataset$n?
      # TODO(mmorin): replace this stacking with a Kronecker product
      x_prior <- matrix(rep(t(dataset$x),
                            n_centering_model_samples / dataset$n),
                        ncol = ncol(dataset$x),
                        byrow = TRUE)

      # Generate classes from features, see page 7 of the paper:
      # "When generating synthetic samples for the posterior bootstrap, both
      #  features and classes are needed. Classes are generated, given features,
      #  according to the probability specified by the logistic distribution."
      probs <- e1071::sigmoid(cbind(1, x_prior) %*% gamma_i)
      y_prior <- stats::rbinom(n = n_centering_model_samples,
                               size = 1, prob = probs)

      # Compute Dirichlet weights
      wgt_mean <- c(rep(1, dataset$n),
                    rep(concentration / n_centering_model_samples,
                        n_centering_model_samples))
      wgts <- stats::rgamma(n = dataset$n + n_centering_model_samples,
                            shape = wgt_mean,
                            rate = rep(1, dataset$n +
                                            n_centering_model_samples))
      x_all <- rbind(dataset$x, x_prior)
      # TODO(mmorin): stop this juggling of y-values between {-1, 1} and {0, 1}
      y_all <- c(0.5 + 0.5 * dataset$y, y_prior)
    } else {
      # No prior samples. Compute Dirichlet weights
      wgt_mean <- rep(1, dataset$n)
      wgts <- stats::rgamma(n = dataset$n,
                            shape = wgt_mean,
                            rate = rep(1, dataset$n))
      x_all <- dataset$x
      y_all <- 0.5 + 0.5 * dataset$y
    }
    stopifnot(all(y_all %in% c(0, 1)))
    # Parameter is computed via weighted glm fit.  We use quasibinomial family
    # instead of binomial to allow the count parameters to be
    # non-integers. Quasibinomial is the same as binomial except that it removes
    # the integer check and does not compute AIC. See the answer by the
    # author (mmorin) on this StackOverflow thread for more details:
    # https://stackoverflow.com/questions/12953045
    glm_fit <- stats::glm.fit(x = cbind(1, x_all),
                              y = y_all,
                              weights = wgts,
                              family = stats::quasibinomial(link = "logit"))

  if (!is.null(progress_bar)) {
    utils::setTxtProgressBar(progress_bar, i)
  }

    return(glm_fit$coefficients)
}


# Plotting detail. We can ignore.
# TODO(mmorin): fix ellipsis in the middle of the arguments
stat_density_2d1 <- function(mapping = NULL,
                             data = NULL,
                             geom = "density_2d",
                             position = "identity",
                             ...,
                             contour = TRUE,
                             n = 100,
                             h = NULL,
                             na_rm = FALSE,
                             show_legend = NA,
                             inherit_aes = TRUE) {
    ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = stat_density_2d1_proto,
      geom = geom,
      position = position,
      show.legend = show_legend,
      inherit.aes = inherit_aes,
      params = list(
        na.rm = na_rm,
        contour = contour,
        n = n,
        h = h,
        ...
      )
    )
}

# Plotting detail. We can ignore.
stat_density_2d1_proto <- ggplot2::ggproto("stat_density_2d1_proto",
  ggplot2::Stat,
  default_aes = ggplot2::aes(colour = "#3366FF", size = 0.5),
  required_aes = c("x", "y"),

  compute_group = function(data, scales, h = NULL,
                           contour = TRUE, n = 100, bins = NULL,
                           binwidth = NULL) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x) * 1.5,
             MASS::bandwidth.nrd(data$y) * 1.5)
    }

    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )
    df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))
    df$group <- data$group[1]

    if (contour) {
      ggplot2::StatContour$compute_panel(df, scales, bins, binwidth)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)

append_to_plot <- function(plot_df, sample, method,
                           n_bootstrap, concentration) {
  new_plot_df <- rbind(plot_df, tibble::tibble(id = 1:n_bootstrap,
                                               beta3 = sample[, 3],
                                               beta5 = sample[, 5],
                                               beta21 = sample[, 21],
                                               beta22 = sample[, 22],
                                               Method = method,
                                               concentration = concentration))
  return(new_plot_df)
}

#' Wrapper function for the script part of the code.
#'
#' Note: this function takes several hours to run on a mac laptop.
#'
#' @param use_bayes_logit Whether to use this package or the alternative
#'   from Kaspar Martens
#' @param verbose Whether to print statements
#'
#' @importFrom Rcpp cpp_object_initializer
#' @export
script <- function(use_bayes_logit = TRUE, verbose = TRUE) {
# Stickbreaking plot
utils::timestamp()
base_font_size <- 8
n_bootstrap <- 1000
concentrations <- c(1, 1000, 20000)
prior_variance <- 100
set.seed(1)

# Load the dataset
dataset <- load_dataset(list(name = k_german_credit))

# Get Bayes (Polson samples)
if (use_bayes_logit) {
  p0 <- diag(rep(1 / prior_variance, dataset$n_cov + 1))
  bayes_out <- BayesLogit::logit(y = 0.5 * (dataset$y + 1),
                                 X = cbind(1, dataset$x),
                                 P0 = p0,
                                 samp = n_bootstrap,
                                 burn = n_bootstrap)
  bayes_sample <- bayes_out$beta
} else {
  bayes_sample <- PolyaGamma::gibbs_sampler(y = 0.5 * (dataset$y + 1),
                                          X = cbind(1, dataset$x),
                                          lambda = 1 / prior_variance,
                                          n_iter_total = 2 * n_bootstrap,
                                          burn_in = n_bootstrap)
}

# Add in Stan VB
train_dat <- list(n = dataset$n,
                  p = dataset$n_cov + 1,
                  x = cbind(1, dataset$x),
                  y = 0.5 * (dataset$y + 1),
                  beta_sd = sqrt(prior_variance))
# Run VB approx from STAN
# the number of samples is the same as the final bootstrap samples,
# as the Stan VB samples serve for the MDP stick-breaking algorithm
bayes_logit_model <- rstan::stan_model(file = get_rstan_file())
stan_vb <- rstan::vb(bayes_logit_model,
                     data = train_dat,
                     output_samples = n_bootstrap,
                     seed = 123)
stan_vb_sample <- rstan::extract(stan_vb)$beta
plot_df <- tibble::tibble()
# Get MDP samples for various sample sizes
for (concentration in concentrations) {
  if (verbose) {
    print(paste0("Sampling at concentration = ", concentration))
  }
  anpl_sample <- anpl(dataset = dataset,
                      concentration = concentration,
                      n_bootstrap = n_bootstrap,
                      posterior_sample = stan_vb_sample,
                      threshold = 1e-8,
                      show_progress = verbose)

  # Append to plot
  plot_df  <- append_to_plot(plot_df, sample = anpl_sample,
                             method = "MDP-VB",
                             n_bootstrap = n_bootstrap,
                             concentration = concentration)
  plot_df  <- append_to_plot(plot_df, sample = bayes_sample,
                             method = "Bayes",
                             n_bootstrap = n_bootstrap,
                             concentration = concentration)
  plot_df  <- append_to_plot(plot_df, sample = stan_vb_sample,
                             method = "VB_Stan",
                             n_bootstrap = n_bootstrap,
                             concentration = concentration)
}

gplot2 <- ggplot2::ggplot(ggplot2::aes_string(x = "beta21",
                                              y = "beta22",
                                              colour = "Method"),
                          data = dplyr::filter(
                            plot_df, plot_df$Method != "Bayes",
                            plot_df$Method != "VB")) +
  stat_density_2d1(bins = 5) +
  ggplot2::geom_point(alpha = 0.1, size = 1,
                      data = dplyr::filter(plot_df,
                                           plot_df$Method == "Bayes",
                                           plot_df$id < 1001)) +
  ggplot2::facet_wrap(~concentration, nrow = 1,
                      scales = "fixed",
                      labeller = ggplot2::label_bquote(c ~" = "~
                                                         .(concentration))
                      ) +
  ggplot2::theme_grey(base_size = base_font_size) +
  ggplot2::xlab(expression(beta[21])) +
  ggplot2::ylab(expression(beta[22])) +
  ggplot2::theme(legend.position = "none",
                 plot.margin = ggplot2::margin(0, 10, 0, 0, "pt"))

ggplot2::ggsave(
  paste0("../190516 vb_logit_scatter_sb (bayesLogit = ",
         use_bayes_logit,
         ").pdf"),
  plot = gplot2, width = 14, height = 5, units = "cm")

#save.image("data/workspace_pcb_vb_classn_sb.RData")
}

# Produce the image with script(use_bayes_logit = {T, F})
