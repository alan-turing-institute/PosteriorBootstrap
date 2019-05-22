

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

}

# Produce the image with script(use_bayes_logit = {T, F})

