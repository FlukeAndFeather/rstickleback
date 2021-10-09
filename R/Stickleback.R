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
#' @slot seed integer. Random number seed.
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
            seed = "integer",
            .stickleback = "ANY")
)

#' @param tsc Python time series classifier. Must inherit sktime's BaseEstimator.
#' @param win_size integer. Sliding window size.
#' @param tol numeric. Prediction tolerance, in seconds.
#' @param nth integer. Sliding window step size.
#' @param n_folds integer. Number of folds for global cross validation step.
#' @slot seed integer. Random number seed.
#'
#' @rdname Stickleback
#' @export
Stickleback <- function(tsc, win_size, tol, nth = 1, n_folds = 4, seed = NULL) {
  stopifnot(inherits(tsc, "sktime.base._base.BaseEstimator"),
            is.numeric(win_size), length(win_size) == 1,
            is.numeric(tol), length(tol) == 1,
            tol > 0, is.finite(tol), !is.na(tol),
            is.numeric(nth), length(nth) == 1,
            nth > 0, is.finite(nth), !is.na(nth), nth < win_size,
            is.numeric(nth), length(nth) == 1,
            n_folds > 0, is.finite(n_folds), !is.na(n_folds),
            (is.null(seed) || (is.numeric(seed) && length(seed) == 1)))

  .stickleback <- .sbenv$sb$Stickleback(
    tsc,
    as.integer(win_size),
    .sbenv$util$timedelta(paste0(tol,"S")),
    as.integer(nth),
    as.integer(n_folds),
    as.integer(seed)
  )

  new("Stickleback",
      local_clf = tsc,
      win_size = as.integer(win_size),
      tol = tol,
      nth = as.integer(nth),
      n_folds = as.integer(n_folds),
      .stickleback = .stickleback)
}

#' Fit a Stickleback model
#'
#' @param sb [Stickleback]
#' @param sensors [Sensors]
#' @param events [Events]
#'
#' @export
sb_fit <- function(sb, sensors, events) {
  stopifnot(inherits(sb, "Stickleback"),
            inherits(sensors, "Sensors"),
            inherits(events, "Events"))
  invisible(sb@.stickleback$fit(sensors@.data, events@.data))
}

#' Predict with a Stickleback model
#'
#' @param sb [Stickleback]
#' @param sensors [Sensors]
#'
#' @export
sb_predict <- function(sb, sensors) {
  stopifnot(inherits(sb, "Stickleback"),
            inherits(sensors, "Sensors"))
  new("Predictions",
      .data = sb@.stickleback$predict(sensors@.data))
}

#' Assess Stickleback predictions
#'
#' @param sb [Stickleback]
#' @param predicted [Predictions]
#' @param events [Events]
#'
#' @return [Outcomes]
#' @export
sb_assess <- function(sb, predicted, events) {
  stopifnot(inherits(sb, "Stickleback"),
            inherits(predicted, "Predictions"),
            inherits(events, "Events"))
  new("Outcomes",
      .data = sb@.stickleback$assess(predicted@.data, events@.data))
}
