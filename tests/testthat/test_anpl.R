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

  # The calculation of the expected speedup depends on the number of bootstrap
  # samples and the number of processors. On Travis, we are limited to 2 cores,
  # so that limits the comparison. In addition, the speedup depends on the
  # system: it is larger on macOS than on Linux, with some variation depending
  # on the version of R.
  #
  # On a macOS machine, I ran n_boostrap from 10 to 1000 by steps of 100, and 3
  # tries for each value, and timed the duration of the code, then ran a linear
  # regression
  #
  # t \approx a + b * n_boostrap
  #
  # for 1 or 2 cores separately. I found an overhead of around 0.13 seconds
  # (intercept a) for both 1 and 2 cores. Each draw takes 0.01186 seconds on one
  # core and 0.006357 on two cores (slope b). The maximum speedup is 1.86, which
  # is Amdahl's law for 2 processors, i.e. 92% of the code in `mcmapply` is
  # parallelisable (= 2 * (1.85 - 1) / 1.85, for s = 2 and S_latency = 1.85).
  #
  # On Travis, the running times are different, so I only verify that the
  # speedup increases with the sample size and that the last speedup is greater
  # than 1.2

  german <- load_dataset(list(name = k_german_credit))

  n_bootstrap <- c(10, 100, 1000)
  speedups <- c()

  for (n_bootstrap in c(10, 100, 1000)) {
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
    speedups <- c(speedups, speedup)
  }

  n_speedups <- length(speedups)
  expect_true(all(speedups[1:n_speedups - 1] < speedups[2:n_speedups]),
              "Parallelization speedup increases with sample size")
  expect_true(speedups[n_speedups] > 1.2,
              "Largest parallelization speedup is larger than 20%")
})
