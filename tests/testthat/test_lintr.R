context("Require lintr to pass")
library(lintr)

test_that("No lintr errors in package", {
    lintr::expect_lint_free()
})
