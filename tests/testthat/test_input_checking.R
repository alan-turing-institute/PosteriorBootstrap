context("Input checking on the German statlog and the generated toy datasets")
library(PosteriorBootstrap)


test_that("German statlog data outcomes are in {1, 2}", {
  raw_dataset <- as.matrix(utils::read.table(get_german_credit_file()))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1, 2)))
})

test_that("German statlog data have all expected fields", {
  german <- get_german_credit_dataset()
  expected_names <- c("n", "n_cov", "x", "y")
  expect_equal(length(setdiff(names(german), expected_names)), 0)
  expect_equal(length(setdiff(expected_names, names(german))), 0)
})

test_that("German statlog data fields are consistent", {
  german <- get_german_credit_dataset()
  expect_equal(dim(german$x), c(german$n, german$n_cov))
  expect_true(all(german$y %in% c(-1, 1)))
})
