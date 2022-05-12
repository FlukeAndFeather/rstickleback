#' @include generics.R
NULL

#' Stickleback class
#'
#' Define a Stickleback model, used for automated detection of behavioral events
#' in bio-logging data.
#'
#' There are two challenges facing automated behavioral event detection in
#' bio-logging data. First, bio-logging data are time series and most
#' classification algorithms have poor performance on time series. Second,
#' bio-logging data resolution greatly exceeds the frequency of many biological
#' rates, creating an imbalanced class problem. For example, bio-logging data
#' collected from baleen whales is often standardized at 10 Hz, but feeding
#' rates are approximately 200-500 events per day. Therefore, the "behavioral
#' event" class is on the order of 1000s times smaller than the "non-event"
#' class.
#'
#' Stickleback addresses these challenges in a two-stage process. First, it uses
#' classification algorithms specifically designed for time series data by
#' interfacing with the [sktime](https://www.sktime.org/en/stable/) Python
#' package. Second, it under-samples the majority class ("non-events") when
#' training the classifier, then optimizes event prediction using internal
#' cross-validation. See `vignette(rstickleback)` for more details.
#'
#' @slot local_clf `[py:sktime.base.BaseEstimator]` A time series classifier,
#'   inheriting from sktime's BaseEstimator.
#' @slot win_size `[integer(1)]` Sliding window size.
#' @slot tol `[numeric(1)]` Prediction tolerance, in seconds.
#' @slot nth `[integer(1)]` Sliding window step size.
#' @slot n_folds `[integer(1)]` Number of folds for global cross validation
#'   step.
#' @slot seed `[integer(1)]` Random number seed.
#' @slot .stickleback `[py:Stickleback]` Python Stickleback object.
#'
#' @examples
#' # Load sample data
#' c(lunge_sensors, lunge_events) %<-% load_lunges()
#' # Define a time series classifier
#' tsc <- compose_tsc(module = "interval_based",
#'                    algorithm = "SupervisedTimeSeriesForest",
#'                    params = list(n_estimators = 2L, random_state = 4321L),
#'                    columns = columns(lunge_sensors))
#' # Define a Stickleback model
#' sb <- Stickleback(tsc,
#'                   win_size = 50,
#'                   tol = 5,
#'                   nth = 10,
#'                   n_folds = 4,
#'                   seed = 1234)
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

#' @param tsc `[py:sktime.base.BaseEstimator]` A time series classifier created
#'   with either \code{\link{compose_tsc}} or \code{\link{create_tsc}}.
#' @param win_size `[integer(1)]` Sliding window size in number of observations.
#'   E.g., for 10 Hz data and a 5 s sliding window, `win_size` should be 50.
#' @param tol `[numeric(1)]` Prediction tolerance, in seconds. See
#'   \code{\link{sb_assess}} for details.
#' @param nth `[integer(1)]` Sliding window step size. For example, when `nth` =
#'   1, the time series classifier (`tsc`) will make predictions on every
#'   window. When `nth` = 2, `tsc` predictions are only generated for every
#'   other window. Higher `nth` values reduce the time to fit a Stickleback
#'   model and generate predictions, at the potential cost of reduced prediction
#'   accuracy.
#' @param n_folds `[integer(1)]` Number of folds for internal cross validation.
#'   `n_folds` must be at least 2. Larger `n_folds` values increase model
#'   fitting time, but may have greater out-of-sample accuracy.
#' @param seed `[integer(1)]` Random number seed for model reproducibility.
#'   CURRENTLY NOT WORKING (see [issue
#'   #6](https://github.com/FlukeAndFeather/rstickleback/issues/6)).
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
    local_clf = tsc,
    win_size = as.integer(win_size),
    tol = .sbenv$util$timedelta(paste0(tol,"S")),
    nth = as.integer(nth),
    n_folds = as.integer(n_folds),
    seed = as.integer(seed)
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
#' @param sb Stickleback
#' @param sensors Sensors
#' @param events Events
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
#' @param sb Stickleback
#' @param sensors Sensors
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
#' @param sb Stickleback
#' @param predicted Predictions
#' @param events Events
#'
#' @return Outcomes
#' @export
sb_assess <- function(sb, predicted, events) {
  stopifnot(inherits(sb, "Stickleback"),
            inherits(predicted, "Predictions"),
            inherits(events, "Events"))
  new("Outcomes",
      .data = sb@.stickleback$assess(predicted@.data, events@.data))
}
