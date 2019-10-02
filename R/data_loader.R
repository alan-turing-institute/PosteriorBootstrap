# This file contains constants and functions relating to data that ships with
# the package: `get_file` and `get_stan_file` for the Stan model of Bayesian
# logistic regression. Note that functions relating to the dataset that ships
# with the package are in their own file (`german_data_loader.R`).

k_extdata <- "extdata"
k_package <- "PosteriorBootstrap"
k_stan_model <- "bayes_logit.stan"

get_file <- function(name) {
  return(system.file(k_extdata, name, package = k_package))
}

#' Get the Stan file with Bayesian Logistic Regression
#'
#' @return An RStan file with the model for variational Bayes that ships with
#'   this package (extension \code{.stan}).
#'
#' @examples
#' f <- get_stan_file()
#' writeLines(readLines(f))
#'
#' @export
get_stan_file <- function() {
  return(get_file(k_stan_model))
}
