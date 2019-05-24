context("Stick-breaking function")

test_that("Stick-breaking function adds up to one", {
  weights <- stick_breaking()
  expect_equal(sum(weights), 1)
})

test_that("Stick-breaking output has a multiple size of input", {
  # Such a multiple is required in `anpl_single`, where `x_prior` needs to be
  # stacked collated multiple times so the number of columns equals the number
  # of stick breaks
  for (min_stick_breaks in seq(2, 20)) {
    weights <- stick_breaking(min_stick_breaks = min_stick_breaks)
    expect_equal(0, length(weights) %% min_stick_breaks)
  }
})
