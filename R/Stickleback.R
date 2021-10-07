#' @include generics.R
NULL

#' Stickleback class
#'
#' R wrapper class for the Python implementation of the Stickleback method, used
#' for automated detection of behavioral events in bio-logging data.
#'
#' Create a Stickleback object with Stickleback(). Fit it to labeled data with
#' fit(). Predict on new data with predict().
#'
#' @slot local_clf Python time series classifier. Must inherit sktime's
#'   BaseEstimator.
#' @slot win_size integer. Sliding window size.
#' @slot tol numeric. Prediction tolerance, in seconds.
#' @slot nth integer. Sliding window step size.
#' @slot n_folds integer. Number of folds for global cross validation step.
#' @slot .stickleback Python Stickleback object.
#'
#' @rdname Stickleback
setClass(
  "Stickleback",
  slots = c(local_clf = "ANY",
            win_size = "integer",
            tol = "numeric",
            nth = "integer",
            n_folds = "integer",
            .stickleback = "ANY")
)

#' @param tsc Python time series classifier. Must inherit sktime's BaseEstimator.
#' @param win_size integer. Sliding window size.
#' @param tol numeric. Prediction tolerance, in seconds.
#' @param nth integer. Sliding window step size.
#' @param n_folds integer. Number of folds for global cross validation step.
#'
#' @rdname Stickleback
#' @export
Stickleback <- function(tsc, win_size, tol, nth = 1, n_folds = 4) {
  stopifnot(inherits(tsc, "sktime.base._base.BaseEstimator"),
            is.numeric(win_size), length(win_size) == 1,
            is.numeric(tol), length(tol) == 1,
            tol > 0, is.finite(tol), !is.na(tol),
            is.numeric(nth), length(nth) == 1,
            nth > 0, is.finite(nth), !is.na(nth), nth < win_size,
            is.numeric(nth), length(nth) == 1,
            n_folds > 0, is.finite(n_folds), !is.na(n_folds))

  .stickleback <- .sbenv$sb$Stickleback(
    tsc,
    as.integer(win_size),
    .sbenv$util$timedelta(tol),
    as.integer(nth),
    as.integer(n_folds)
  )

  new("Stickleback",
      local_clf = tsc,
      win_size = as.integer(win_size),
      tol = tol,
      nth = as.integer(nth),
      n_folds = as.integer(n_folds),
      .stickleback = .stickleback)
}

#' @param sb Stickleback object.
#' @param sensors Sensors object.
#' @param events Events object.
#'
#' @rdname Stickleback
#' @export
setMethod("fit",
          c("Stickleback", "Sensors", "Events"),
          function(sb, sensors, events) {
            sb@.stickleback$fit(sensors@.data, events@.data)
            NULL
          })

#' @param sb Stickleback object.
#' @param sensors Sensors object.
#'
#' @rdname Stickleback
#' @export
setMethod("predict",
          c("Stickleback", "Sensors"),
          function(sb, sensors) {
            sb@.stickleback$predict(sensors@.data)
            NULL
          })
