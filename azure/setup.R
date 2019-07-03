lib = "~/R/"
dir.create(lib)
.libPaths(c(lib, .libPaths()))
options(repos=structure(c(CRAN = "https://cran.r-project.org")))
packages <- c("devtools", "dplyr", "e1071", "ggplot2", "gridExtra", "MASS", "Rcpp",
              "rstan", "utils", "tibble", "knitr", "lintr", "rmarkdown",
              "roxygen2", "testthat")
for (p in packages) {
  install.packages(p)
}
