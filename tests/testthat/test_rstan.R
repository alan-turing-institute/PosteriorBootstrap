context("RStan variational Bayes model")
library("PosteriorBootstrap")

test_that("Rstan variational Bayes model runs", {

  german <- get_german_credit_dataset()
  n_bootstrap <- 10
  prior_variance <- 100

  stan_vb_sample <- run_variational_bayes(x = german$x,
                                          y = german$y,
                                          output_samples = n_bootstrap,
                                          beta_sd = sqrt(prior_variance),
                                          iter = 10)
  expect_true(nrow(stan_vb_sample) == n_bootstrap)
  expect_true(ncol(stan_vb_sample) == ncol(german$x))
})
