k_extdata <- "extdata"
k_package <- "PosteriorBootstrap"
k_stan_model <- "bayes_logit.stan"

data_file <- function(name) {
  return(system.file(k_extdata, name, package = k_package))
}

#' Stan file with Bayesian Logistic Regression.
#'
#' @return An RStan file with the model for variational Bayes that ships with
#'   this package (extension \code{.stan}).
#'
#' @examples
#' f <- get_stan_file()
#' \dontrun{
#' file.show(f)
#' }
#'
#' @export
get_stan_file <- function() {
  return(data_file(k_stan_model))
}
