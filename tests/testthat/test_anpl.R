context("Adaptive non-parametric learning function")

# Rstan needs to be loaded and attached, not just loaded. See
# https://stackoverflow.com/questions/56262828/
library("rstan")

test_that("Adaptive non-parametric learning avoids bad inputs", {
  german <- get_german_credit_dataset()
  x <- german$x
  y <- german$y
  n_cov <- ncol(german$x)
  gamma_mean <- rep(0, n_cov)
  gamma_vcov <- diag(1, n_cov)

  # Both gamma_mean and gamma_vcov need to be present
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_vcov = gamma_vcov),
               regexp = paste0("If you don't provide a posterior sample, you ",
                               "must provide a mean for the centering model"))
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_mean = gamma_mean),
               regexp = paste0("If you don't provide a posterior sample, you ",
                               "must provide a variance-covariance for the ",
                               "centering model"))

  # Both gamma_mean and gamma_vcov need to be numeric
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_mean = "string",
                                  gamma_vcov = gamma_vcov),
               regexp = paste0("Invalid input: the mean and variance-",
                               "covariance of the centering model need ",
                               "to be numeric"))
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_mean = gamma_mean,
                                  gamma_vcov = "string"),
               regexp = paste0("Invalid input: the mean and variance-",
                               "covariance of the centering model need ",
                               "to be numeric"))

  # Both gamma_mean and gamma_vcov need the right dimensions
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_mean = rep(0, n_cov - 1),
                                  gamma_vcov = gamma_vcov),
               regexp = paste0("You need to give a vector for the mean ",
                  "of the centering model with size "))
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  gamma_mean = gamma_mean,
                                  gamma_vcov = diag(1, n_cov - 1)),
               regexp = paste0("You need to give a matrix for the variance-",
                               "covariance matrix of the centering model: "))

  # The posterior sample needs to have the right dimensions
  n_bootstrap <- 1000
  posterior_sample1 <- matrix(0, ncol = n_cov - 1, nrow = n_bootstrap)
  posterior_sample2 <- matrix(0, ncol = n_cov, nrow = n_bootstrap - 1)
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  n_bootstrap = n_bootstrap,
                                  posterior_sample = posterior_sample1),
               regexp = paste0("The number of columns in the posterior sample ",
                               "must be the same as the number of covariates"))
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                  n_bootstrap = n_bootstrap,
                                  posterior_sample = posterior_sample2),
               regexp = paste0("The posterior sample must have a number of ",
                               "rows no smaller than n_bootstrap")
               )

  # Concentration
  expect_error(draw_logit_samples(x = x, y = y, concentration = "string",
                                  gamma_mean = gamma_mean,
                                  gamma_vcov = gamma_vcov),
               regexp = "Concentration needs to be numeric")
  expect_error(draw_logit_samples(x = x, y = y, concentration = -1,
                                  gamma_mean = gamma_mean,
                                  gamma_vcov = gamma_vcov),
               regexp = "Concentration needs to be positive or zero")

  # Outcome values in (0, 1)
  y[1] <- -1
  expect_error(draw_logit_samples(x = x, y = y, concentration = 1,
                                   gamma_mean = gamma_mean,
                                   gamma_vcov = gamma_vcov),
               regexp = "The values of y must all be in \\(0, 1\\)")
})

test_that("Adaptive non-parametric learning with centering model works", {

  german <- get_german_credit_dataset()
  n_cov <- ncol(german$x)

  n_bootstrap <- 10

  for (concentration in c(0, 1)) {
    anpl_samples <- draw_logit_samples(x = german$x,
                                       y = german$y,
                                       concentration = concentration,
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
  }
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

  durations <- list()
  for (i in c(1, 2)) {
    start <- Sys.time()
    anpl_samples <- do.call(draw_logit_samples, c(list(num_cores = i), params))
    durations[[i]] <- as.double(Sys.time() - start, units = "secs")
    print(sprintf("Duration with %1.0f core(s): %4.4f s", i, durations[[i]]))
  }

  speedup <- durations[[1]] / durations[[2]]
  print(sprintf("Speedup: %3.2f (1 = same duration)", speedup))

  # From tests on macOS on a local machine and Linux on a virtual machine, the
  # speedups for n = 1000 vary between 1.75 and 1.89. So 1.6 seems a reasonable
  # number on macOS. On Linux, the speedups vary between 1.5 and 1.56, so 1.4
  # seems a reasonable number for now. In the future, revisit this "magic
  # number" as needed.
  if ("Darwin" == Sys.info()["sysname"]) {
    expected_speedup <- 1.6
  } else {
    expected_speedup <- 1.4
  }
  expect_true(speedup > expected_speedup,
              "Parallelization speedup is as expected")
})

test_that("Adaptive non-parametric learning with posterior samples works", {
  # On Windows RStan fails with a known error:
  # C++14 standard requested but CXX14 is not defined
  # The necessary fix is to rebuild RStan, but we cannot do that on remote
  # systems like CRAN or Rhub so we skip the test instead
  skip_on_os(c("windows"))

  german <- get_german_credit_dataset()

  n_bootstrap <- 100

  # Get posterior samples
  seed <- 123
  prior_sd <- 10
  train_dat <- list(n = length(german$y), p = ncol(german$x), x = german$x,
                    y = german$y, beta_sd = prior_sd)
  stan_model <- rstan::stan_model(file = get_stan_file())
  # Suppress Pareto k diagnostic warnings
  suppressWarnings(
    stan_vb <- rstan::vb(object = stan_model, data = train_dat, seed = seed,
                         output_samples = n_bootstrap)
  )
  stan_vb_sample <- rstan::extract(stan_vb)$beta

  # Use these samples in ANPL
  anpl_samples <- draw_logit_samples(x = german$x,
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

test_that("Adaptive non-parametric learning communicates progress bar", {
  german <- get_german_credit_dataset()
  n_cov <- ncol(german$x)
  n_bootstrap <- 10
  expect_output(anpl_samples <- draw_logit_samples(x = german$x,
                                                   y = german$y,
                                                   concentration = 1,
                                                   n_bootstrap = n_bootstrap,
                                                   gamma_mean = rep(0, n_cov),
                                                   gamma_vcov = diag(1, n_cov),
                                                   show_progress = TRUE), "=")
})
