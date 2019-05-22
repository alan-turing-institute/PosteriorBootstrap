context("Adaptive non-parametric learning function")
library(parallel)
library(PosteriorBootstrap)
library(rstan)

test_that("Adaptive non-parametric learning with centering model works", {

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

  # The following two tests relate to using the result of `mcmapply`. If it's
  # used as a list (like the result of `mclapply`) instead of a matrix, either
  # the columns or the rows will all be the same.
  expect_true(all(diag(var(anpl_samples)) > 0),
              "Row values in ANPL samples are different")
  expect_true(all(diag(var(t(anpl_samples))) > 0),
              "Column values in ANPL samples are different")

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
  # number on macOS. On Linux, the speedups vary between 1.5 and 1.56, so 1.4
  # seems a reasonable number for now. In the future, revisit this "magic
  # number" as needed.
  if ("Darwin" == Sys.info()["sysname"]) {
    expected_speedup <- 1.7
  } else {
    expected_speedup <- 1.4
  }
  expect_true(speedup > expected_speedup,
              "Largest parallelization speedup is larger than 70%")
})

test_that("Adaptive non-parametric learning with posterior samples works well", {

  german <- load_dataset(list(name = k_german_credit))
  n_bootstrap <- 100

  # Get posterior samples
  prior_variance <- 100
  bayes_logit_model <- rstan::stan_model(file = get_rstan_file())
  train_dat <- list(n = german$n,
                    p = german$n_cov + 1,
                    x = cbind(1, german$x),
                    y = 0.5 * (german$y + 1),
                    beta_sd = sqrt(prior_variance))

  set.seed(1)
  stan_vb <- rstan::vb(bayes_logit_model,
                       data = train_dat,
                       output_samples = n_bootstrap,
                       seed = 123,
                       iter = 100)
  stan_vb_sample <- rstan::extract(stan_vb)$beta

  # Use these samples in ANPL with multiple cores
  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       posterior_sample = stan_vb_sample,
                       threshold = 1e-8,
                       num_cores = 2)
  
  # Once we got a problem with coefficients way off because of misuse of
  # `mcmapply` (as if it were a list instead of a matrix). So we added a test
  # for the average value of the coefficients in the paper for the specific
  # magic numbers in this tests, where reproducibility ensues from `set.seed()`.
  col_means <- colMeans(anpl_samples)
  print(col_means[21])
  print(col_means[22])
  expect_true((col_means[21] >= 0.1) && (col_means[21] <= 0.4),
              "The average coefficient for column 21 is as expected")
  expect_true((col_means[22] >= -0.3) && (col_means[22] <= -0),
              "The average coefficient for column 22 is as expected")
})
