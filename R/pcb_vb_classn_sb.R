## Code generating figure 2 in Lyddon, Walker & Holmes, 2018.
options(warn = 1)

requireNamespace("BayesLogit", quietly = TRUE)
requireNamespace("dplyr", quietly = TRUE)
requireNamespace("e1071", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("MASS", quietly = TRUE)
requireNamespace("PolyaGamma", quietly = TRUE)
requireNamespace("stats", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("utils", quietly = TRUE)
library(rstan)

k_extdata <- "extdata"
k_package <- "PosteriorBootstrap"
k_german_credit <- "statlog-german-credit.dat"
k_rstan_model <- "bayes_logit.stan"

data_file <- function(name) {
  return(system.file(k_extdata, name, package = k_package))
}

get_rstan_file <- function() {
  return(data_file(k_rstan_model))
}

get_german_credit_file <- function() {
  return(data_file(k_german_credit))
}

# Function to load dataset. Won't be necessary in the package
load_dataset <- function(dataset_input_list = list(name = "toy",
                                                   n = 200,
                                                   pct_train = 0.5)) {
  # Check dataset_input_list has required elements
  if (!all(c("name", "pct_train") %in% names(dataset_input_list))) {
    stop("dataset_input_list does not contain all of (name, pct_train)")
  }

  # dataset object is list we will return.
  dataset <- list(name = dataset_input_list$name,
                  pct_train = dataset_input_list$pct_train)

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
  dataset$obs <- 1:dataset$n
  dataset$x <- raw_dataset[, 1:dataset$n_cov, drop = FALSE]

  # Convert y to +/- 1 format
  dataset$y <- 2 * raw_dataset[, dataset$n_cov + 1] - 1
  # Generate the training dataset and add to the list.
  dataset$n_train <- floor(dataset$n * dataset$pct_train)
  dataset$obs_train <- sample.int(n = dataset$n,
                                  size = dataset$n_train,
                                  replace = FALSE)
  dataset$x_train <- dataset$x[dataset$obs_train, , drop = FALSE]
  dataset$y_train <- dataset$y[dataset$obs_train]
  # Generate the test dataset and add to the list.
  dataset$n_test <- dataset$n - dataset$n_train
  dataset$obs_test <- dataset$obs[!(dataset$obs %in% dataset$obs_train)]
  dataset$x_test <- dataset$x[dataset$obs_test, , drop = FALSE]
  dataset$y_test <- dataset$y[dataset$obs_test]
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
#'   stops looking for more stick-breaks. It correspondes to epsilon in the
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
#' @return A matrix of bootstrap samples for the parameter of interest
#'
#' @export
anpl <- function(dataset,
                 concentration,
                 n_bootstrap = 100,
                 posterior_sample = NULL,
                 gamma_mean = NULL,
                 gamma_vcov = NULL,
                 threshold = 1e-8) {

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


  # Create matrix with which to store posterior samples
  theta_out <- matrix(0, nrow = n_bootstrap, ncol = dataset$n_cov + 1)
  # Get mixing theta
  if (is.null(posterior_sample)) {
    gamma <- MASS::mvrnorm(n = n_bootstrap,
                           mu = gamma_mean,
                           Sigma = gamma_vcov)
  } else {
    gamma <- posterior_sample
  }
  # Generate prior samples

  # `x_prior` is a `n_bootstrap x n_centering_model_samples` matrix:
  # each row represents a set of prior samples for a particular gamma element.

  # This for loop can be be parallelised
  for (i in 1:n_bootstrap) {
    if (concentration != 0) {
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
                                    min_stick_breaks = dataset$n_train,
                                    threshold = threshold)
      w_model <- w_raw_model * s_i
      n_centering_model_samples <- length(w_model)
      if (i == 1) {
        print(paste("n_centering_model_samples = ", n_centering_model_samples))
      }

      # Create prior samples
      # Prior means "model"
      x_prior <- matrix(rep(t(dataset$x_train),
                            n_centering_model_samples / dataset$n_train),
                        ncol = ncol(dataset$x_train),
                        byrow = TRUE)

      # Generate classes from features, see page 7 of the paper:
      # "When generating synthetic samples for the posterior bootstrap, both
      #  features and classes are needed. Classes are generated, given features,
      #  according to the probability specified by the logistic distribution."
      probs <- e1071::sigmoid(cbind(1, x_prior) %*% gamma[i, ])
      y_prior <- stats::rbinom(n = n_centering_model_samples,
                               size = 1, prob = probs)

      # Compute Dirichlet weights
      wgt_mean <- c(rep(1, dataset$n_train),
                    rep(concentration / n_centering_model_samples,
                        n_centering_model_samples))
      wgts <- stats::rgamma(n = dataset$n_train + n_centering_model_samples,
                            shape = wgt_mean,
                            rate = rep(1, dataset$n_train +
                                            n_centering_model_samples))
      x_all <- rbind(dataset$x_train, x_prior)
      # TODO(mmorin): stop this juggling of y-values between {-1, 1} and {0, 1}
      y_all <- c(0.5 + 0.5 * dataset$y_train, y_prior)
    } else {
      # No prior samples. Compute Dirichlet weights
      wgt_mean <- rep(1, dataset$n_train)
      wgts <- stats::rgamma(n = dataset$n_train,
                            shape = wgt_mean,
                            rate = rep(1, dataset$n_train))
      x_all <- dataset$x_train
      y_all <- 0.5 + 0.5 * dataset$y_train
    }
    # Parameter is computed via weighted glm fit.  We use quasibinomial family
    # instead of binomial to allow the count parameters to be
    # non-integers. Quasibinomial is the same as binomial except that it removes
    # the integer check and does not compute AIC. See the answer by the
    # author (mmorin) on this StackOverflow thread for more details:
    # https://stackoverflow.com/questions/12953045
    stopifnot(all(y_all %in% c(0, 1)))
    glm_fit <- stats::glm.fit(x = cbind(1, x_all),
                              y = y_all,
                              weights = wgts,
                              family = stats::quasibinomial(link = "logit"))
    theta_out[i, ] <- glm_fit$coefficients
  }
  return(theta_out)
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
stat_density_2d1_proto <- ggproto("stat_density_2d1_proto", Stat,
  default_aes = aes(colour = "#3366FF", size = 0.5),
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
      StatContour$compute_panel(df, scales, bins, binwidth)
    } else {
      names(df) <- c("x", "y", "density", "group")
      df$level <- 1
      df$piece <- 1
      df
    }
  }
)


#' @importFrom Rcpp cpp_object_initializer
script <- function(use_bayes_logit, verbose=TRUE) {
# Stickbreaking plot
utils::timestamp()
base_font_size <- 8
pct_train <- 1
n_bootstrap <- 1000
concentrations <- c(1, 1000, 20000)
prior_variance <- 100
set.seed(1)

# Load the dataset
dataset <- load_dataset(list(name = k_german_credit,
                              pct_train = pct_train))
# Get Bayes (Polson samples)
if (use_bayes_logit) {
  p0 <- diag(rep(1 / prior_variance, dataset$n_cov + 1))
  out_bayes1 <- BayesLogit::logit(y = 0.5 * (dataset$y_train + 1),
                                  X = cbind(1, dataset$x_train),
                                  P0 = p0,
                                  samp = n_bootstrap,
                                  burn = n_bootstrap)
} else {
  out_bayes1 <- PolyaGamma::gibbs_sampler(y = 0.5 * (dataset$y_train + 1),
                                          X = cbind(1, dataset$x_train),
                                          lambda = 1 / prior_variance,
                                          n_iter_total = 2 * n_bootstrap,
                                          burn_in = n_bootstrap)
}

# Add in Stan VB
train_dat <- list(n = dataset$n_train,
                  p = dataset$n_cov + 1,
                  x = cbind(1, dataset$x_train),
                  y = 0.5 * (dataset$y_train + 1),
                  beta_sd = sqrt(prior_variance))
# Run VB approx from STAN
# the number of samples is the same as the final bootstrap samples,
# as the Stan VB samples serve for the MDP stick-breaking algorithm
bayes_logit_model <- rstan::stan_model(file = get_rstan_file())
out_vb_stan <- rstan::vb(bayes_logit_model,
                         data = train_dat,
                         output_samples = n_bootstrap,
                         seed = 123)
stan_vb_sample <- rstan::extract(out_vb_stan)$beta
plot_df1 <- tibble::tibble()
# Get MDP samples for various sample sizes
for (concentration in concentrations) {
  tmp_mdp_out <- anpl(dataset = dataset,
                      concentration = concentration,
                      n_bootstrap = n_bootstrap,
                      posterior_sample = stan_vb_sample,
                      threshold = 1e-8)

  # TODO(MMorin): refactor with a helper called three times
  plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_bootstrap,
                                             beta3 = tmp_mdp_out[, 3],
                                             beta5 = tmp_mdp_out[, 5],
                                             beta21 = tmp_mdp_out[, 21],
                                             beta22 = tmp_mdp_out[, 22],
                                             Method = "MDP-VB",
                                             concentration = concentration))
  plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_bootstrap,
                                             beta3 = out_bayes1$beta[, 3],
                                             beta5 = out_bayes1$beta[, 5],
                                             beta21 = out_bayes1$beta[, 21],
                                             beta22 = out_bayes1$beta[, 22],
                                             Method = "Bayes",
                                             concentration = concentration))
  plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_bootstrap,
                                             beta3 = stan_vb_sample[, 3],
                                             beta5 = stan_vb_sample[, 5],
                                             beta21 = stan_vb_sample[, 21],
                                             beta22 = stan_vb_sample[, 22],
                                             Method = "VB_Stan",
                                             concentration = concentration))
  print(summary(tmp_mdp_out[, c(21, 22)]))
}

gplot2 <- ggplot2::ggplot(ggplot2::aes_string(x = "beta21",
                                              y = "beta22",
                                              colour = "Method"),
                          data = dplyr::filter(
                            plot_df1, plot_df1$Method != "Bayes",
                            plot_df1$Method != "VB")) +
  stat_density_2d1(bins = 5) +
  ggplot2::geom_point(alpha = 0.1, size = 1,
                      data = dplyr::filter(plot_df1,
                                           plot_df1$Method == "Bayes",
                                           plot_df1$id < 1001)) +
  ggplot2::facet_wrap(~prior_sample_size, nrow = 1,
                      scales = "fixed",
                      labeller = ggplot2::label_bquote(c ~" = "~
                                                         .(prior_sample_size))
                      ) +
  ggplot2::theme_grey(base_size = base_font_size) +
  ggplot2::xlab(expression(beta[21])) +
  ggplot2::ylab(expression(beta[22])) +
  ggplot2::theme(legend.position = "none",
                 plot.margin = ggplot2::margin(0, 10, 0, 0, "pt"))

ggplot2::ggsave(
  paste0("vb_logit_scatter_sb (bayesLogit = ",
         use_bayes_logit,
         ").pdf"),
  plot = gplot2, width = 14, height = 5, units = "cm")

save.image("data/workspace_pcb_vb_classn_sb.RData")
}

# Produce the image with script(use_bayes_logit = {T, F})
