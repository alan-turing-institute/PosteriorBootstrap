context("Input checking on the German statlog and the generated toy datasets")
library(PosteriorBootstrap)

test_that("German statlog data outcomes are in {1, 2}", {
  data_file <- system.file("extdata",
                           "statlog-german-credit.dat",
                           package = "PosteriorBootstrap")
  raw_dataset <- as.matrix(utils::read.table(data_file))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1, 2)))
})

test_that("Toy dataset outcomes are in {-1, 1}", {
  toy <- load_dataset()
  expect_true(all(toy$y_train %in% c(-1, 1)))
  expect_true(all(toy$y_test %in% c(-1, 1)))
})
