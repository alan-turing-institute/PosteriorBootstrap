.libPaths(c("~/R/x86_64-pc-linux-gnu-library/3.4", .libPaths()))
options(repos=structure(c(CRAN = "https://cran.r-project.org")))
packages <- c("devtools", "dplyr", "e1071", "ggplot2", "gridExtra", "MASS", "Rcpp",
              "rstan", "utils", "tibble", "knitr", "lintr", "rmarkdown", "testthat")
for (p in packages) {
  install.packages(p)
}
