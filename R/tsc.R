#' Create a time series classification model
#'
#' Time series classification algorithms are not widely available in R, so
#' Stickleback interfaces with a Python package (sktime). These helper functions
#' define time series classification models using the sktime package. Use
#' `create_tsc` for univariate bio-logging data and `compose_tsc` for
#' multivariate.
#'
#' @param module `[character(1)]` Name of module in
#'   [sktime.classification](https://www.sktime.org/en/stable/api_reference/classification.html),
#'    e.g. "interval_based".
#' @param algorithm `[character(1)]` Name of time series classification
#'   algorithm, e.g. "SupervisedTimeSeriesForest".
#' @param params `[list]` Hyperparameters for algorithm, e.g. number of
#'   estimators.
#' @param columns `[character(1)]` Names of columns for composition
#'   (`compose_tsc` only).
#'
#' @return `[py:sktime.base.BaseEstimator]` A time series classification model
#'   (see \code{\link{Stickleback}}).
#' @export
#'
#' @examples
#' # Load sample data
#' c(lunge_sensors, lunge_events) %<-% load_lunges()
#' # Define a time series classifier
#' tsc <- compose_tsc(module = "interval_based",
#'                    algorithm = "SupervisedTimeSeriesForest",
#'                    params = list(n_estimators = 2L, random_state = 4321L),
#'                    columns = columns(lunge_sensors))
create_tsc <- function(module, algorithm, params) {
  full_module <- paste("sktime.classification", module, sep = ".")
  tsc_mod <- reticulate::import(full_module, convert = FALSE)
  do.call(tsc_mod[[algorithm]], params)
}

#' @rdname create_tsc
#' @export
compose_tsc <- function(module, algorithm, params, columns) {
  tsc <- create_tsc(module, algorithm, params)
  .sbenv$util$compose_tsc(tsc, columns)
}

