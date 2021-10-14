#' Create a time series classification model.
#'
#' @param module \[character(1)\] Name of module in sktime.classification, e.g.
#'   "interval_based".
#' @param algorithm \[character(1)\] Name of time series classification algorithm.
#' @param params \[list\] Hyperparameters to algorithm.
#'
#' @return A time series classification model, for use in fitting a Stickleback
#'   object.
#' @export
create_tsc <- function(module, algorithm, params) {
  full_module <- paste("sktime.classification", module, sep = ".")
  tsc_mod <- reticulate::import(full_module, convert = FALSE)
  do.call(tsc_mod[[algorithm]], params)
}

#' Compose time series classification model across multiple variables.
#'
#' @param module \[character(1)\] Name of module in sktime.classification, e.g.
#'   "interval_based".
#' @param algorithm \[character(1)\] Name of time series classification algorithm.
#' @param params \[list\] Hyperparameters to algorithm.
#' @param columns \[character\] Names of columns for composition.
#'
#' @return A time series classification model, for use in fitting a Stickleback
#'   object.
#' @export
compose_tsc <- function(module, algorithm, params, columns) {
  tsc <- create_tsc(module, algorithm, params)
  .sbenv$util$compose_tsc(tsc, columns)
}

