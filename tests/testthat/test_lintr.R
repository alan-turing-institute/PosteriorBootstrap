context("Code style errors")
library("lintr")

test_that("Package has no lintr errors", {
  lintr::expect_lint_free()
})
