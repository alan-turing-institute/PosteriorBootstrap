context("German Statlog credit dataset")

test_that("German Statlog data outcomes are in {1, 2}", {
  raw_dataset <- as.matrix(utils::read.table(get_german_credit_file()))
  expect_true(all(raw_dataset[, ncol(raw_dataset)] %in% c(1, 2)))
})

test_that("German Statlog data have all expected fields", {
  german <- get_german_credit_dataset()
  expected_names <- c("x", "y")
  expect_equal(length(setdiff(names(german), expected_names)), 0)
  expect_equal(length(setdiff(expected_names, names(german))), 0)
})

test_that("German Statlog data fields are consistent", {
  german <- get_german_credit_dataset()
  expect_equal(nrow(german$x), length(german$y))
  expect_true(all(german$y %in% c(0, 1)))
})

test_that("German Statlog data loads properly", {
  german <- get_german_credit_dataset(scale = FALSE, add_constant_term = FALSE)
  num_attributes <- 24
  expect_true(num_attributes == ncol(german$x))
  expect_true(all(german$y[1:5] == c(0, 1, 0, 0, 1)))
  expect_true(all(german$x[1, 1:5] == c(1, 6, 4, 12, 5)))
  expect_true(all(german$x[2, 1:5] == c(2, 48, 2, 60, 1)))

  german <- get_german_credit_dataset(scale = FALSE, add_constant_term = TRUE)
  expect_true(num_attributes + 1 == ncol(german$x))
  expect_true(all(german$x[, 1] == 1))
})

test_that("German Statlog data that ships is the same as source", {
  # This tests the availability of the remote German credit data server
  # As failure here is out of our control we do not want it to:
  # a) cause removal from CRAN
  # b) cause our CI to fail
  skip_on_cran()
  skip_on_ci()

  local <- get_german_credit_dataset(scale = FALSE)
  remote <- get_german_credit_dataset(scale = FALSE,
                                      download_destination = tempfile())
  expect_true(all(local$x == remote$x))
  expect_true(all(local$y == remote$y))
})
