#' The 'PosteriorBootstrap' package.
#'
#' @description What the Package Does (One Line, Title Case)
#'
#' @docType package
#' @name PosteriorBootstrap
#' @aliases PosteriorBootstrap
#' @importFrom rstan stan_model vb extract

## Code generating figure 2 in Lyddon, Walker & Holmes, 2018.
options(warn = 1)

requireNamespace("utils", quietly = TRUE)
requireNamespace("stats", quietly = TRUE)
requireNamespace("tidyverse", quietly = TRUE)
requireNamespace("MASS", quietly = TRUE)
requireNamespace("e1071", quietly = TRUE)
requireNamespace("BayesLogit", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
# requireNamespace("rstan", quietly = TRUE)
library(rstan)

# TODO: get the data, the model, and update the paths
dataset_path <- file.path('inst', 'extdata')


# Function to load dataset. Won't be necessary in the package
load_dataset <- function(dataset_input_list = list(name = 'toy', n = 200, pct_train = 0.5)) {
  # Check dataset_input_list has required elements
  if (!all(c('name', 'pct_train') %in% names(dataset_input_list))) {
    stop('dataset_input_list does not contain all of (name, pct_train)')
  }
  # dataset object is list we will return.
  dataset <- list(name = dataset_input_list$name, pct_train = dataset_input_list$pct_train)
  if (dataset$name == 'toy') {
    if (!('n' %in% names(dataset_input_list))) stop('dataset_input_list does not contain n')
    # Generate the dataset
    raw_dataset <- rbinom(n = dataset_input_list$n, size = 1, prob = 0.5) # Gives values 0 or 1
    raw_dataset <- cbind(rnorm(n = dataset_input_list$n, mean = raw_dataset * 2 - 1, sd = 1), raw_dataset)
  } else {
    # Load the dataset
    raw_dataset <- as.matrix(utils::read.table(file.path(dataset_path, dataset$name)))
    # German statlog dataset outcomes are (1, 2), so subtract 1 here to make them
    # 0,1, as in the toy dataset
    if ("statlog-german-credit.dat" == dataset_input_list$name) {
      raw_dataset[, ncol(raw_dataset)] <- raw_dataset[, ncol(raw_dataset)] - 1
    }
    colnames(raw_dataset) <- NULL
  }
  # Add the dataset to the dataset output list.
  dataset$n <- dim(raw_dataset)[1]
  dataset$n_cov <- dim(raw_dataset)[2] - 1 # Doesn't include an intercept
  dataset$obs <- 1:dataset$n
  dataset$x <- raw_dataset[, 1:dataset$n_cov, drop = FALSE]
  dataset$y <- 2 * raw_dataset[, dataset$n_cov + 1] - 1 # y is now in +/- 1 format.
  # Generate the training dataset and add to the list.
  dataset$n_train <- floor(dataset$n * dataset$pct_train)
  dataset$obs_train <- sample.int(n = dataset$n, size = dataset$n_train, replace = FALSE)
  dataset$x_train <- dataset$x[dataset$obs_train,, drop = FALSE]
  dataset$y_train <- dataset$y[dataset$obs_train]
  # Generate the test dataset and add to the list.
  dataset$n_test <- dataset$n - dataset$n_train
  dataset$obs_test <- dataset$obs[!(dataset$obs %in% dataset$obs_train)]
  dataset$x_test <- dataset$x[dataset$obs_test,, drop = FALSE]
  dataset$y_test <- dataset$y[dataset$obs_test]
  # Return the dataset object.
  return(dataset)
}

# TODO: robustify -1 and 1, or change the Bayes Logit call

# This code implements algorithm 2, and the stick-breaking function
# calculates the parameter T for algorithm 1. That is the only difference between the two algorithms.
# "Stick-breaking" comes from a stick of length 1 to break into the number of items in the probability
# distribution.

# Function that takes stick-breaking NPL approach (Section 2 of supplementary material)
stick_breaking <- function(par_c = 1, n_start = 100, eps = 10 ^ -8) {
  num_wgts <- n_start / 10
  stick_rem <- 1
  while (stick_rem > eps) {
    num_wgts = num_wgts * 10
    u_s <- rbeta(n = num_wgts, shape1 = 1, shape2 = par_c)
    c_rem <- c(1, cumprod(1 - u_s))
    stick_rem <- c_rem[num_wgts + 1]
  }
  wgts <- c_rem[1:num_wgts] * u_s
  wgts <- wgts / sum(wgts)
  return(wgts)
}


# TODO: change name `prior_sample_size`, which corresponds to c in the paper

# Function that implements the NP-learning algorithm
# Either 'posterior_sample' is passed or the posterior is assumed normal with 'mix_mean' and 'mix_cov' defining it.
# 'prior_sample_size' is the 'c' in the paper
mdp_logit_mvn_stickbreaking <- function(n_samp = 100, mix_mean, mix_cov, posterior_sample = NULL, prior_sample_size, dataset, tol = 1e-8) {
  # Create matrix with which to store posterior samples
  theta_out <- matrix(0, nrow = n_samp, ncol = dataset$n_cov + 1)
  # Get mixing theta
  if (is.null(posterior_sample)) {
    mix_theta <- MASS::mvrnorm(n = n_samp, mu = mix_mean, Sigma = mix_cov)
  } else {
    mix_theta <- posterior_sample
  }
  # Generate prior samples
  # x_prior is B x n_prior matrix: each row represents a set of prior samples for a particular mix_theta element.
  for (i in 1:n_samp) {
    # This for loop can be be parallelised
    if (prior_sample_size != 0) {
      # Get stick-breaking split between data and model
      v1 <- rbeta(n = 1, shape1 = prior_sample_size, shape2 = dataset$n) # v1 is random around c / (c + n)
      # TODO: maybe make v1 deterministic, which only works for algorithm 1, otherwise in algorithm 2
      # the epsilon is just 1/prior_sample_size.
      w_raw_data <- rexp(n = dataset$n)
      w_data <- w_raw_data / sum(w_raw_data) * (1 - v1)

      w_raw_model <- stick_breaking(par_c = prior_sample_size, n_start = dataset$n_train, eps = tol)
      w_model <- w_raw_model * v1
      n_prior <- length(w_model)
      if (i == 1) {
        print(paste('n_prior =', n_prior))
      }

      # Create prior samples
      # Prior means "model"
      x_prior <- matrix(rep(t(dataset$x_train), n_prior / dataset$n_train), ncol = ncol(dataset$x_train), byrow = TRUE)
      probs <- e1071::sigmoid(cbind(1, x_prior) %*% mix_theta[i,])
      y_prior <- stats::rbinom(n = n_prior, size = 1, prob = probs)
      # Compute Dirichlet weights
      wgt_mean <- c(rep(1, dataset$n_train), rep(prior_sample_size / n_prior, n_prior))
      wgts <- stats::rgamma(n = dataset$n_train + n_prior, shape = wgt_mean, rate = rep(1, dataset$n_train + n_prior))
      x_all <- rbind(dataset$x_train, x_prior)
      y_all <- c(0.5 + 0.5 * dataset$y_train, y_prior)
    } else {
      # No prior samples. Compute Dirichlet weights
      wgt_mean <- rep(1, dataset$n_train)
      wgts <- stats::rgamma(n = dataset$n_train, shape = wgt_mean, rate = rep(1, dataset$n_train))
      x_all <- dataset$x_train
      y_all <- 0.5 + 0.5 * dataset$y_train
    }
    # Parameter is computed via weighted glm fit.
    stopifnot(all(y_all %in% c(0, 1)))
    glm_fit <- glm.fit(x = cbind(1, x_all), y = y_all, weights = wgts, family = binomial(link = logit))
    theta_out[i,] <- glm_fit$coefficients
  }
  return(theta_out)
}


# Plotting detail. We can ignore.
stat_density_2d1 <- function(mapping = NULL, data = NULL,
  geom = "density_2d", position = "identity",
  ...,
  contour = TRUE,
  n = 100,
  h = NULL,
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE) {
  layer(
      data = data,
      mapping = mapping,
      stat = StatDensity2d1,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        na.rm = na.rm,
        contour = contour,
        n = n,
        h = h,
        ...
      )
    )
}

# Plotting detail. We can ignore.
StatDensity2d1 <- ggplot2::ggproto("StatDensity2d1", ggplot2::Stat,
  default_aes = ggplot2::aes(colour = "#3366FF", size = 0.5),
  required_aes = c("x", "y"),

  compute_group = function(data, scales, na.rm = FALSE, h = NULL,
                           contour = TRUE, n = 100, bins = NULL,
                           binwidth = NULL) {
    if (is.null(h)) {
      h <- c(MASS::bandwidth.nrd(data$x) * 1.5, MASS::bandwidth.nrd(data$y) * 1.5)
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

script <- function() {

  # Stickbreaking plot
  utils::timestamp()
  base_font_size = 8
  pct_train = 1
  n_samp = 10
  n_samp_mdp <- 10
  prior_sample_sizes <- c(1, 10, 200)
  prior_variance <- 100
  set.seed(1)

  # Load the dataset
  dataset1 <- load_dataset(list(name = 'statlog-german-credit.dat', pct_train = pct_train))
  # Get Bayes (Polson samples)
  out_bayes1 <- BayesLogit::logit(y = 0.5 * (dataset1$y_train + 1), X = cbind(1, dataset1$x_train), P0 = diag(rep(1 / prior_variance, dataset1$n_cov + 1)), samp = n_samp, burn = n_samp)
  #out_VB1 <- logit_VB(dataset1, prior_mean=0, prior_cov = prior_variance, n_samp = n_samp)
  # Add in Stan VB
  train_dat <- list(n = dataset1$n_train, p = dataset1$n_cov + 1, x = cbind(1, dataset1$x_train), y = 0.5 * (dataset1$y_train + 1), beta_sd = sqrt(prior_variance))
  # Run VB approx from STAN
  bayes_logit_model <- rstan::stan_model(file = file.path(dataset_path, 'bayes_logit.stan'))
  out_VB_stan <- rstan::vb(bayes_logit_model, data = train_dat, output_samples = n_samp, seed = 123)
  stan_vb_sample <- rstan::extract(out_VB_stan)$beta
  plot_df1 <- tibble::tibble()
  # Get MDP samples for various sample sizes
  for (i in prior_sample_sizes) {
    tmp_mdp_out <- mdp_logit_mvn_stickbreaking(n_samp = n_samp_mdp, mix_mean = NULL, mix_cov = NULL, posterior_sample = stan_vb_sample[1:n_samp_mdp,], prior_sample_size = i, dataset = dataset1, tol = 1e-8)
    plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_samp_mdp, beta3 = tmp_mdp_out[, 3], beta5 = tmp_mdp_out[, 5], beta21 = tmp_mdp_out[, 21], beta22 = tmp_mdp_out[, 22], Method = 'MDP-VB', prior_sample_size = i))
    #plot_df1 <- rbind(plot_df1, tibble::tibble(id=1:n_samp, beta3=out_VB1$beta[,3], beta5=out_VB1$beta[,5],beta21=out_VB1$beta[,21], beta22=out_VB1$beta[,22], Method='VB', prior_sample_size=i) )
    plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_samp, beta3 = out_bayes1$beta[, 3], beta5 = out_bayes1$beta[, 5], beta21 = out_bayes1$beta[, 21], beta22 = out_bayes1$beta[, 22], Method = 'Bayes', prior_sample_size = i))
    plot_df1 <- rbind(plot_df1, tibble::tibble(id = 1:n_samp, beta3 = stan_vb_sample[, 3], beta5 = stan_vb_sample[, 5], beta21 = stan_vb_sample[, 21], beta22 = stan_vb_sample[, 22], Method = 'VB_Stan', prior_sample_size = i))
    print(summary(tmp_mdp_out[, c(21, 22)]))
  }

  gplot2 <- ggplot(data = dplyr::filter(plot_df1, Method != 'Bayes', Method != 'VB'),
                   ggplot2::aes(x = beta21, y = beta22, colour = Method)) +
              stat_density_2d1(bins = 5) +
              geom_point(data = dplyr::filter(plot_df1, Method == 'Bayes', id < 1001), alpha = 0.1, size = 1) +
              facet_wrap(~prior_sample_size, nrow = 1, scales = "fixed", labeller = label_bquote(c ~ "=" ~ .(prior_sample_size))) +
              theme_grey(base_size = base_font_size) +
              xlab(expression(beta[21])) +
              ylab(expression(beta[22])) +
              theme(legend.position = 'none', plot.margin = margin(0, 10, 0, 0, "pt"))

  ggsave('../vb_logit_scatter_sb.pdf', plot = gplot2, width = 14, height = 5, units = 'cm')

  save.image('../workspace_pcb_vb_classn_sb.RData')
}
