requireNamespace("PosteriorBootstrap", quietly = TRUE)

requireNamespace("dplyr", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("parallel", quietly = TRUE)

german <- PosteriorBootstrap::get_german_credit_dataset()

num_tries <- 10

speedups  <- data.frame(stringsAsFactors = FALSE)
max_cores <- parallel::detectCores(logical=TRUE)
n_bootstrap_array <- c(1e2, 1e3, 1e4)

print("Speedup performance")

for (n_bootstrap in n_bootstrap_array) {
  print(paste0("n_bootstrap = ", n_bootstrap))
  one_core_duration <- NULL
  for (num_cores in seq(1, max_cores)) {
    print(paste0("num_cores = ", num_cores))
    
    start <- Sys.time()
    for (i in 1:num_tries) {
    anpl_samples <- PosteriorBootstrap::draw_logit_samples(x = german$x, y = german$y,
                                                           concentration = 1,
                                                           n_bootstrap = n_bootstrap,
                                                           gamma_mean = rep(0, ncol(german$x)),
                                                           gamma_vcov = diag(1, ncol(german$x)),
                                                           threshold = 1e-8,
                                                           num_cores = num_cores)
    }
    print("Finished sampling")
    lap <- as.double((Sys.time() - start), units = "secs")
    
    if (1 == num_cores) {
      one_core_duration <- lap
    }
    speedups <- rbind(speedups, c(num_cores, n_bootstrap, one_core_duration / lap))
    names(speedups) <- c("Num_cores", "N_bootstrap", "speedup")

    # Save to disk for inspection
    write.csv(speedups, file = "azure/speedups.csv", row.names = FALSE)
  }
}

# Convert n_bootstrap to strings for ggplot2 to arrange them into groups
speedups$N_bootstrap <- paste0("N_", speedups$N_bootstrap)

gplot2 <- ggplot2::ggplot(data = speedups,
                          ggplot2::aes(x = Num_cores, y = speedup)) +
  ggplot2::geom_line(ggplot2::aes(colour = N_bootstrap))
ggplot2::ggsave("Speedup.png", plot = gplot2, width = 8, height = 5.5, units = 'in', dpi = 72)

library("gridExtra")

# Remove single core speedup, where the proportion is not defined
speedups <- speedups[1 != speedups$Num_cores, ]
speedups$proportion <- (1 / speedups$speedup - 1) / (1 / speedups$Num_cores - 1)

# Plot
gplot2 <- ggplot2::qplot(proportion, data = speedups, fill = N_bootstrap, binwidth = 0.005) +
  ggplot2::facet_wrap(facets = ~N_bootstrap) + ggplot2::ylab("Number")
ggplot2::ggsave("Proportion.png", plot = gplot2, width = 8, height = 5.5, units = 'in', dpi = 72)
