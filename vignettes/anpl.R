
setwd("~/Projects/PosteriorBootstrap/PosteriorBootstrap")
load(file = "../samples2.RData")

recompile  <- FALSE
resample <- FALSE

if (recompile) {
  devtools::document();devtools::build();devtools::install();
}
requireNamespace("PosteriorBootstrap")

dataset_path <- file.path("inst", "extdata")

# Define a ggproto to compute the density of samples. The first argument is
# required, either NULL or an arbitrary string.
stat_density_2d1_proto <- ggplot2::ggproto(NULL,
  ggplot2::Stat,
  required_aes = c("x", "y"),

  compute_group = function(data, scales, bins, n) {
    # Choose the bandwidth of Gaussian kernel estimators and increase it for
    # smoother densities in small sample sizes
    h <- c(MASS::bandwidth.nrd(data$x) * 1.5,
           MASS::bandwidth.nrd(data$y) * 1.5)

    # Estimate two-dimensional density
    dens <- MASS::kde2d(
      data$x, data$y, h = h, n = n,
      lims = c(scales$x$dimension(), scales$y$dimension())
    )
    
    # Store in data frame
    df <- data.frame(expand.grid(x = dens$x, y = dens$y), z = as.vector(dens$z))

    # Add a label of this density for ggplot2
    df$group <- data$group[1]

    # plot
    ggplot2::StatContour$compute_panel(df, scales, bins)
  }
)

# Wrap that ggproto in a ggplot2 object
stat_density_2d1 <- function(data = NULL,
                             geom = "density_2d",
                             position = "identity",
                             n = 100,
                             ...) {
    ggplot2::layer(
      data = data,
      stat = stat_density_2d1_proto,
      geom = geom,
      position = position,
      params = list(
        n = n,
        ...
      )
    )
}


append_to_plot <- function(plot_df, sample, method,
                           n_bootstrap, concentration) {
  new_plot_df <- rbind(plot_df, tibble::tibble(beta21 = sample[, 21],
                                               beta22 = sample[, 22],
                                               Method = method,
                                               concentration = concentration))
  return(new_plot_df)
}


use_bayes_logit <- TRUE
verbose <- TRUE

  # Stickbreaking plot
  base_font_size <- 8
  n_bootstrap <- 1000
  concentrations <- c(1)
  prior_variance <- 100
  set.seed(1)

if (resample) {
  # Load the dataset

  dataset <- PosteriorBootstrap::load_dataset(list(name = "statlog-german-credit.dat"))
  
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
  bayes_logit_model <- rstan::stan_model(file = PosteriorBootstrap::get_rstan_file())
  stan_vb <- rstan::vb(bayes_logit_model,
                       data = train_dat,
                       output_samples = n_bootstrap,
                       seed = 123)
  stan_vb_sample <- rstan::extract(stan_vb)$beta

  # Get ANPL samples for various concentrations
  for (concentration in concentrations) {
    anpl_sample <- PosteriorBootstrap::anpl(dataset = dataset,
                                            concentration = concentration,
                                            n_bootstrap = n_bootstrap,
                                            posterior_sample = stan_vb_sample,
                                            threshold = 1e-8)
  }
}

concentration <- concentrations[1]
plot_df <- tibble::tibble()

anpl_name <- "MDP-VB"

  # Append to plot
  plot_df  <- append_to_plot(plot_df, sample = anpl_sample,
                             method = anpl_name,
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


gplot2 <- ggplot2::ggplot(ggplot2::aes_string(x = "beta21",
                                              y = "beta22",
                                              colour = "Method"),
                          data = dplyr::filter(
                            plot_df, plot_df$Method != "Bayes")) +
  stat_density_2d1(bins = 5) +
  ggplot2::geom_point(alpha = 0.1, size = 1,
                      data = dplyr::filter(plot_df,
                                           plot_df$Method == "Bayes")) +
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
  paste0("../plot.pdf"),
  plot = gplot2, width = 14, height = 5, units = "cm")

save(file = "../samples2.RData", list = c("bayes_sample", "stan_vb_sample", "anpl_sample"))
