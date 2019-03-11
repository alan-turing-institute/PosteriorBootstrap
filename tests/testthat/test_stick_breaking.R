context("Stick-breaking function")
library(PosteriorBootstrap)


test_that("Stick-breaking function adds up to one", {
  weights <- stick_breaking()
  expect_equal(sum(weights), 1)
})

# TODO(mmorin): speed up this test after playing with function parameters
