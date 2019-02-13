context("Sample data")
library(PosteriorBootstrap)

test_that("sample raw data outcomes are {1,2}", {
  raw_dataset <- as.matrix(utils::read.table(system.file("extdata", "statlog-german-credit.dat", package="PosteriorBootstrap")))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1,2)))
})

test_that("toy dataset outcomes are {-1,1}", {
  toy <- load_dataset()
  expect_true(all(toy$y_train %in% c(-1,1)))
  expect_true(all(toy$y_test %in% c(-1,1)))
})