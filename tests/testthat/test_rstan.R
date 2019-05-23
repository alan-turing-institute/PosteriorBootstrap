context("RStan variational Bayes model")
library(PosteriorBootstrap)

test_that("Rstan variational Bayes model runs", {

  german <- get_german_credit_dataset()
  n_bootstrap <- 10
  prior_variance <- 100

  stan_vb_sample <- run_variational_bayes(x = cbind(1, german$x),
                                          y = german$y,
                                          output_samples = n_bootstrap,
                                          beta_sd = sqrt(prior_variance),
                                          iter = 10)
  expect_true(dim(stan_vb_sample)[1] == n_bootstrap)
  expect_true(dim(stan_vb_sample)[2] == dim(german$x)[2] + 1)
})
