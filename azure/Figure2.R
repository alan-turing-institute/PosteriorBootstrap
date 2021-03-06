# Code copied from vignette
requireNamespace("PosteriorBootstrap", quietly = TRUE)

requireNamespace("dplyr", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)

library("rstan")

#The first argument is required, either NULL or an arbitrary string.
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
                           concentration, x_index, y_index) {
  new_plot_df <- rbind(plot_df, tibble::tibble(x = sample[, x_index],
                                               y = sample[, y_index],
                                               Method = method,
                                               concentration = concentration))
  return(new_plot_df)
}

seed <- 123
prior_sd <- 10
n_bootstrap <- 1000
german <- PosteriorBootstrap::get_german_credit_dataset(scale = TRUE)

train_dat <- list(n = length(german$y), p = ncol(german$x), x = german$x, y = german$y, beta_sd = prior_sd)
stan_file <- PosteriorBootstrap::get_stan_file()
bayes_log_reg <- rstan::stan(stan_file, data = train_dat, seed = seed,
                      iter = n_bootstrap * 2, chains = 1)
stan_bayes_sample <- rstan::extract(bayes_log_reg)$beta

stan_model <- rstan::stan_model(file = stan_file)
stan_vb <- rstan::vb(object = stan_model, data = train_dat, seed = seed,
                 output_samples = n_bootstrap)
stan_vb_sample <- rstan::extract(stan_vb)$beta

concentrations <- c(1, 1000, 20000)
anpl_samples <- list()
for (concentration in concentrations) {
  anpl_sample <- PosteriorBootstrap::draw_logit_samples(x = german$x, y = german$y,
                                                        concentration = concentration,
                                                        n_bootstrap = n_bootstrap,
                                                        posterior_sample = stan_vb_sample,
                                                        threshold = 1e-8,
                                                        show_progress = TRUE)
  anpl_samples[[toString(concentration)]] <- anpl_sample
}

# Initialise
plot_df <- tibble::tibble()

# Index of coefficients in the plot
x_index <- 21
y_index <- 22

# Create a plot data frame with all the samples
for (concentration in concentrations) {
  plot_df  <- append_to_plot(plot_df, sample = anpl_samples[[toString(concentration)]],
                             method = "PosteriorBootstrap-ANPL",
                             concentration = concentration,
                             x_index = x_index, y_index = y_index)
  plot_df  <- append_to_plot(plot_df, sample = stan_bayes_sample,
                             method = "Bayes-Stan",
                             concentration = concentration,
                             x_index = x_index, y_index = y_index)
  plot_df  <- append_to_plot(plot_df, sample = stan_vb_sample,
                             method = "VB-Stan",
                             concentration = concentration,
                             x_index = x_index, y_index = y_index)
}

gplot2 <- ggplot2::ggplot(ggplot2::aes_string(x = "x", y = "y", colour = "Method"),
                  data = dplyr::filter(plot_df, plot_df$Method != "Bayes-Stan")) +
    stat_density_2d1(bins = 5) +
    ggplot2::geom_point(alpha = 0.1, size = 1,
                        data = dplyr::filter(plot_df,
                                             plot_df$Method == "Bayes-Stan")) +
    ggplot2::facet_wrap(~concentration, nrow = 1,
                        scales = "fixed",
                        labeller = ggplot2::label_bquote(c ~" = "~
                                                           .(concentration))
                        ) +
    ggplot2::theme_grey(base_size = 8) +
    ggplot2::xlab(bquote(beta[.(x_index)])) +
    ggplot2::ylab(bquote(beta[.(y_index)])) +
    ggplot2::theme(legend.position = "none",
                   plot.margin = ggplot2::margin(0, 10, 0, 0, "pt"))


ggplot2::ggsave("Figure2.png", plot = gplot2, width = 14, height = 5, units = 'cm', dpi = 300)
