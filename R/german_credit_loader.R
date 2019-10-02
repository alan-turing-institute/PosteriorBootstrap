# This file contains constants and functions relating to the dataset that ships
# with the package: `get_german_credit_file` and `get_german_credit_dataset`.

k_german_credit <- "statlog-german-credit.dat"
k_german_credit_url <- paste0("http://archive.ics.uci.edu/ml/",
                              "machine-learning-databases/statlog/",
                              "german/german.data-numeric")

#' Get the file with the German Statlog credit dataset
#'
#' The file contains a local copy of the German Statlog credit dataset with
#' 1,000 observations and 24 features. The data page is at:
#' https://archive.ics.uci.edu/ml/datasets/statlog+(german+credit+data) and the
#' original files at:
#' http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/ We
#' use the file `german.data-numeric`, which has 24 covariates instead of the 20
#' in the original data (as some are qualitative).
#'
#' @return A file with the plain-text raw data for the German Statlog credit
#'   that ships with this package (extension \code{.dat}).
#'
#' @examples
#' f <- get_german_credit_file()
#' writeLines(readLines(f, n=5))
#'
#' @export
get_german_credit_file <- function() {
  # This call uses the `get_file` function from `R/data_loader.R`
  return(get_file(k_german_credit))
}

#' Load and pre-process the dataset that ships with the package
#'
#' @param scale Whether to scale the features to have mean 0 and variance 1.
#' @param add_constant_term Whether to add a constant term as the first feature.
#' @param download_destination Provide a filepath if you want to download the
#'   dataset from source. Note that although the original dataset has 20
#'   features (some of them qualitative), the numeric dataset has 24 features.
#'
#' @return A list with fields \code{x} for features and \code{y} for outcomes.
#'
#' @examples
#' \donttest{
#' german <- get_german_credit_dataset()
#' head(german$y)
#' head(german$x)
#' }
#'
#' @export
get_german_credit_dataset <- function(scale = TRUE, add_constant_term = TRUE,
                                      download_destination = NULL) {

  if (is.null(download_destination)) {
    filepath <- get_german_credit_file()
  } else {
    utils::download.file(k_german_credit_url, download_destination)
    filepath <- download_destination
  }
  raw_dataset <- as.matrix(utils::read.table(filepath))
  colnames(raw_dataset) <- NULL

  x <- raw_dataset[, 1:(ncol(raw_dataset) - 1)]
  y <- raw_dataset[, ncol(raw_dataset)]

  # German statlog dataset outcomes are (1, 2), so
  # subtract 1 here to make them (0, 1)
  y <- y - 1
  stopifnot(all(y %in% c(0, 1)))

  # Standardise features to have mean 0 and variance 1
  if (scale) {
    x <- scale(x)
  }
  if (add_constant_term) {
    x <- cbind(1, x)
  }

  # Return the list object
  return(list(x = x, y = y))
}
