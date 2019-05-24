requireNamespace("PosteriorBootstrap", quietly = TRUE)

requireNamespace("dplyr", quietly = TRUE)
requireNamespace("ggplot2", quietly = TRUE)
requireNamespace("tibble", quietly = TRUE)
requireNamespace("parallel", quietly = TRUE)

n_bootstrap <- 1000
german <- PosteriorBootstrap::get_german_credit_dataset()

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
    anpl_samples <- PosteriorBootstrap::draw_logit_samples(x = german$x, y = german$y,
                                             concentration = 1,
                                             n_bootstrap = n_bootstrap,
                                             gamma_mean = rep(0, ncol(german$x)),
                                             gamma_vcov = diag(1, ncol(german$x)),
                                             threshold = 1e-8,
                                             num_cores = num_cores)
    print("Finished sampling")
    lap <- as.double((Sys.time() - start), units = "secs")
    
    if (1 == num_cores) {
	  one_core_duration <- lap
    }
    speedups <- rbind(speedups, c(num_cores, n_bootstrap, one_core_duration / lap))
  }
  names(speedups) <- c("Num_cores", "N_bootstrap", "speedup")
}

# Convert n_bootstrap to strings for ggplot2 to arrange them into groups
speedups$N_bootstrap <- paste0("N_", speedups$N_bootstrap)

png("Speedup.png", width = 400, height = 300)
ggplot2::ggplot(data = speedups,
                ggplot2::aes(x = Num_cores, y = speedup)) +
  ggplot2::geom_line(ggplot2::aes(colour = N_bootstrap))
dev.off()

library("gridExtra")

# Remove single core speedup, where the proportion is not defined
speedups <- speedups[1 != speedups$Num_cores, ]
speedups$proportion <- (1 / speedups$speedup - 1) / (1 / speedups$Num_cores - 1)


png("Proportion.png", width = 400, height = 300)
ggplot2::qplot(proportion, data = speedups, fill = N_bootstrap, binwidth = 0.005) +
  ggplot2::facet_wrap(facets = ~N_bootstrap)
dev.off()
