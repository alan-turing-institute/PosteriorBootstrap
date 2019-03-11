context("Input checking on the German statlog and the generated toy datasets")
library(PosteriorBootstrap)


test_that("German statlog data outcomes are in {1, 2}", {
  raw_dataset <- as.matrix(utils::read.table(k_german_credit_file))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1, 2)))
})

test_that("Toy dataset outcomes are in {-1, 1}", {
  toy <- load_dataset()
  expect_true(all(toy$y_train %in% c(-1, 1)))
  expect_true(all(toy$y_test %in% c(-1, 1)))
})

names_helper <- function(dataset) {
  expected_names <- c("name", "pct_train",
                      "n", "n_cov", "n_train", "n_test",
                      "obs", "x", "y",
                      "obs_train", "x_train", "y_train",
                      "obs_test", "x_test", "y_test")
  expect_equal(length(setdiff(names(dataset), expected_names)), 0)
  expect_equal(length(setdiff(expected_names, names(dataset))), 0)
}

test_that("German statlog and toy data have all expected fields", {
  toy <- load_dataset()
  german <- load_dataset(list(name = k_german_credit,
                              pct_train = 1))
  names_helper(toy)
  names_helper(german)
})
