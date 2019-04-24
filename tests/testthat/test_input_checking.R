context("Input checking on the German statlog and the generated toy datasets")
library(PosteriorBootstrap)


test_that("German statlog data outcomes are in {1, 2}", {
  raw_dataset <- as.matrix(utils::read.table(get_german_credit_file()))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1, 2)))
})

test_that("Toy dataset outcomes are in {-1, 1}", {
  toy <- load_dataset()
  expect_true(all(toy$y_train %in% c(-1, 1)))
  expect_true(all(toy$y_test %in% c(-1, 1)))
})

names_helper <- function(dataset) {
  expected_names <- c("name", "n", "n_cov", "x", "y")
  expect_equal(length(setdiff(names(dataset), expected_names)), 0)
  expect_equal(length(setdiff(expected_names, names(dataset))), 0)
}

test_that("German statlog and toy data have all expected fields", {
  toy <- load_dataset()
  german <- load_dataset(list(name = k_german_credit))
  names_helper(toy)
  names_helper(german)
})

consistency_helper <- function(dataset) {
  expect_equal(dim(dataset$x), c(dataset$n, dataset$n_cov))
  expect_true(all(dataset$y %in% c(-1, 1)))
}

test_that("German statlog and toy data fields are consistent", {
  toy <- load_dataset()
  german <- load_dataset(list(name = k_german_credit))
  consistency_helper(toy)
  consistency_helper(german)
})
