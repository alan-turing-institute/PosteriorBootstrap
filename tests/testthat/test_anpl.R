context("Adaptive non-parametric learning function")
library(PosteriorBootstrap)

# TODO(mmorin): speed up this test after playing with function parameters

test_that("Mixture of Dirichlet Processes stick-breaking works and returns", {

  german <- load_dataset(list(name = k_german_credit))

  n_bootstrap <- 10

  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8)

  expect_equal(dim(anpl_samples), c(n_bootstrap, 1 + german$n_cov))
})

test_that("Parallelisation works and is faster", {
  german <- load_dataset(list(name = k_german_credit))

  n_bootstrap <- 10

  start <- Sys.time()
  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8,
                       num_cores = 1)
  one_core_duration <- (Sys.time() - start)

  start <- Sys.time()
  anpl_samples <- anpl(dataset = german,
                       concentration = 1,
                       n_bootstrap = n_bootstrap,
                       gamma_mean = rep(0, german$n_cov + 1),
                       gamma_vcov = diag(1, german$n_cov + 1),
                       threshold = 1e-8,
                       num_cores = 2)
  two_cores_duration <- Sys.time() - start

  print(one_core_duration)
  print(two_cores_duration)
  expect_true(one_core_duration >= 1.25 * two_cores_duration,
              "Parallelisation is faster")

})
