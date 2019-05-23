context("Adaptive non-parametric learning function")
library("parallel")
library("PosteriorBootstrap")
library("rstan")

test_that("Adaptive non-parametric learning with centering model works", {

  german <- get_german_credit_dataset()
  n_cov <- ncol(german$x)

  n_bootstrap <- 10

  anpl_samples <- anpl(x = german$x,
                       y = german$y,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, n_cov),
                       gamma_vcov = diag(1, n_cov),
                       threshold = 1e-8)

  expect_true(is.numeric(anpl_samples))
  expect_equal(dim(anpl_samples), c(n_bootstrap, n_cov))

  # The following two tests relate to using the result of `mcmapply`. If it's
  # used as a list (like the result of `mclapply`) instead of a matrix, either
  # the columns or the rows will all be the same.
  expect_true(all(diag(var(anpl_samples)) > 0),
              "Row values in ANPL samples are different")
  expect_true(all(diag(var(t(anpl_samples))) > 0),
              "Column values in ANPL samples are different")

})

test_that("Multiple processors are available", {
  skip_on_cran()
  num_cores <- parallel::detectCores(logical = FALSE)
  print(paste0("Physical cores available: ", num_cores))
  expect_true(num_cores >= 2, "Multiple processors are available")
})

test_that("Parallelisation works and is faster", {
  skip_on_cran()

  german <- get_german_credit_dataset()
  n_cov <- ncol(german$x)

  n_bootstrap <- 1000

  params <- list(x = german$x,
                 y = german$y,
                 concentration = 1,
                 n_bootstrap = n_bootstrap,
                 gamma_mean = rep(0, n_cov),
                 gamma_vcov = diag(1, n_cov),
                 threshold = 1e-8)

  start <- Sys.time()
  anpl_samples <- do.call(anpl, c(list(num_cores = 1), params))
  one_core_duration <- as.double(Sys.time() - start, units = "secs")

  start <- Sys.time()
  anpl_samples <- do.call(anpl, c(list(num_cores = 2), params))
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

test_that("Adaptive non-parametric learning with posterior samples works", {

  german <- get_german_credit_dataset()

  n_bootstrap <- 100

  # Get posterior samples
  prior_variance <- 100
  stan_vb_sample <- run_variational_bayes(x = german$x,
                                          y = german$y,
                                          output_samples = n_bootstrap,
                                          beta_sd = sqrt(prior_variance))

  # Use these samples in ANPL with multiple cores
  anpl_samples <- anpl(x = german$x,
                       y = german$y,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       posterior_sample = stan_vb_sample,
                       threshold = 1e-8)

  # Once we got a problem with coefficients way off because of misuse of
  # `mcmapply` (as if it were a list instead of a matrix). So we added a test
  # for the average value of the coefficients in the paper for the specific
  # magic numbers in this tests, where reproducibility ensues from `set.seed()`.
  col_means <- colMeans(anpl_samples)
  print(col_means[21])
  print(col_means[22])
  ok21 <- (col_means[21] >= 0.05) && (col_means[21] <= 0.4)
  ok22 <- (col_means[22] >= -0.3) && (col_means[22] <= 0)
  expect_true(ok21, "The average coefficient for column 21 is as expected")
  expect_true(ok22, "The average coefficient for column 22 is as expected")
})
