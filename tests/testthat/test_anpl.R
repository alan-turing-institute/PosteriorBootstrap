context("Adaptive non-parametric learning function")
library(PosteriorBootstrap)
library(parallel)

test_that("Mixture of Dirichlet Processes stick-breaking works and returns", {

  german <- load_dataset(list(name = k_german_credit))

  n_bootstrap <- 10

  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8)

  expect_true(is.numeric(anpl_samples))
  expect_equal(dim(anpl_samples), c(n_bootstrap, 1 + german$n_cov))
})

test_that("Multiple processors are available", {
  num_cores <- parallel::detectCores(logical = FALSE)
  print(paste0("Physical cores available: ", num_cores))
  expect_true(num_cores >= 2, "Multiple processors are available")
})

test_that("Parallelisation works and is faster", {

  german <- load_dataset(list(name = k_german_credit))

  n_bootstrap <- 1000

  start <- Sys.time()
  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8,
                       num_cores = 1)
  one_core_duration <- as.double((Sys.time() - start), units = "secs")

  start <- Sys.time()
  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8,
                       num_cores = 2)
  two_cores_duration <- as.double(Sys.time() - start, units = "secs")
  speedup <- one_core_duration / two_cores_duration

  print(sprintf("Duration with 1 core: %4.4f s", one_core_duration))
  print(sprintf("Duration with 2 cores: %4.4f s", two_cores_duration))
  print(sprintf("Speedup: %3.2f (1 = same duration)", speedup))

  # From tests on macOS on a local machine and Linux on a virtual machine, the
  # speedups for n = 1000 vary between 1.75 and 1.89. So 1.7 seems a reasonable
  # number, but in the future, revisit this "magic number" as needed.
  expect_true(speedup > 1.7,
              "Largest parallelization speedup is larger than 70%")
})
