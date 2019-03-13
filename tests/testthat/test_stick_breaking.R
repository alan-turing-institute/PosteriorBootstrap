context("Stick-breaking function")
library(PosteriorBootstrap)


test_that("Stick-breaking function adds up to one", {
  weights <- stick_breaking()
  expect_equal(sum(weights), 1)
})

# TODO(mmorin): speed up this test after playing with function parameters

test_that("Mixture of Dirichlet Processes stick-breaking works and returns", {

  german <- load_dataset(list(name = k_german_credit,
                              pct_train = 1))

  n_samp <- 100
  prior_variance <- 100

  train_dat <- list(n = german$n_train,
                    p = german$n_cov + 1,
                    x = cbind(1, german$x_train),
                    y = 0.5 * (german$y_train + 1),
                    beta_sd = sqrt(prior_variance))

  stan_file <- data_file("bayes_logit.stan")
  bayes_logit_model <- rstan::stan_model(file = stan_file)
  out_vb_stan <- rstan::vb(bayes_logit_model,
                           data = train_dat,
                           output_samples = n_samp,
                           seed = 123)
  stan_vb_sample <- rstan::extract(out_vb_stan)$beta
  stan_sample <- stan_vb_sample[1:n_samp, ]

  mdp_samples <- mdp_logit_mvn_stickbreaking(n_samp = n_samp,
                                             mix_mean = NULL,
                                             mix_cov = NULL,
                                             posterior_sample = stan_sample,
                                             prior_sample_size = 1,
                                             dataset = german,
                                             tol = 1e-8)

  expect_equal(dim(mdp_samples), c(n_samp, 1 + german$n_cov))
})
