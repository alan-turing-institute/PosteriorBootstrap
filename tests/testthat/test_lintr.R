context("Code style errors")
requireNamespace("lintr", quietly = TRUE)

test_that("Package has no lintr errors", {
  lintr::expect_lint_free()
})
