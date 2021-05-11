if (requireNamespace("lintr", quietly = TRUE)) {
  context("Code style errors")

  test_that("Package has no lintr errors", {
    lintr::expect_lint_free()
  })
}