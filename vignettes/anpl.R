## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
requireNamespace("PosteriorBootstrap", quietly = TRUE)
requireNamespace("rstan", quietly = TRUE)

## ------------------------------------------------------------------------
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

## ------------------------------------------------------------------------
append_to_plot <- function(plot_df, sample, method,
                           concentration, x_index, y_index) {
  new_plot_df <- rbind(plot_df, tibble::tibble(x = sample[, x_index],
                                               y = sample[, y_index],
                                               Method = method,
                                               concentration = concentration))
  return(new_plot_df)
}

## ------------------------------------------------------------------------
prior_variance <- 100
n_bootstrap <- 1000
set.seed(1)
dataset <- PosteriorBootstrap::load_dataset(list(name = "statlog-german-credit.dat"))

