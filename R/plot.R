#' Plot bio-logging data
#'
#' The high resolution of bio-logging sensor data relative to the duration of
#' deployments limits the utility of static figures. `sb_plot_data` and
#' `sb_plot_predictions` use [plotly](https://plotly.com/r/) to create
#' interactive figures for looking in-depth at bio-logging data.
#'
#' @param deployid `[character(1)]` ID of the deployment to visualize. Must be a
#'   deployment in both `sensors` and `events`.
#' @param sensors `[Sensors]` Bio-logging sensor data (see
#'   \code{\link{Sensors}}).
#' @param events `[Events]` Labeled behavioral events (see
#'   \code{\link{Events}}).
#' @param predictions `[Predictions]` Predicted behavioral events (see
#'   \code{\link{sb_predict}}).
#' @param outcomes `[Outcomes]` Outcomes of predicted behavioral events (see
#'   \code{\link{sb_assess}}) (optional).
#'
#' @return An interactive figure with time on the x-axis and a separate y-axis
#'   for  each bio-logging sensor variable, arranged vertically. Interaction
#'   options include: click-and-drag to zoom in, double-click to zoom out, hover
#'   to see details (exact date, time, and value).
#'
#'   ## `sb_plot_data`
#'   Points indicate known behavioral events.
#'
#'   ##  `sb_plot_predictions`
#'   Solid points indicate predicted behavioral events. If actual events are
#'   known (i.e, `outcomes` is not NULL), they are plotted as hollow circles and
#'   color-coded as follows: true positive as blue solid point in blue hollow
#'   point, false positive as red solid point, false negative as red hollow
#'   point. In addition to the bio-logging sensor variables, there's an
#'   additional time series for the "local probability" of an event (see
#'   `vignette(TODO)`).
#'
#' @examples
#' # Load sample data and split test/train
#' c(lunge_sensors, lunge_events) %<-% load_lunges()
#' test_deployids <- deployments(lunge_sensors)[1:3]
#' deployid <- test_deployids[1]
#' c(sensors_test, sensors_train) %<-% divide(lunge_sensors, test_deployids)
#' c(events_test, events_train) %<-% divide(lunge_events, test_deployids)
#'
#' # Visualize sensor and event data for one deployment
#' sb_plot_data(deployid, lunge_sensors, lunge_events)
#'
#' # Define a Stickleback model
#' tsc <- compose_tsc(module = "interval_based",
#'                    algorithm = "SupervisedTimeSeriesForest",
#'                    params = list(n_estimators = 2L, random_state = 4321L),
#'                    columns = columns(lunge_sensors))
#' sb <- Stickleback(tsc,
#'                   win_size = 50,
#'                   tol = 5,
#'                   nth = 10,
#'                   n_folds = 4,
#'                   seed = 1234)
#'
#' # Fit the model to the training data, make predictions on the test data, and
#' # assess the outcomes of the predictions
#' sb_fit(sb, sensors_train, events_train)
#' predictions <- sb_predict(sb, sensors_test)
#' outcomes <- sb_assess(sb, predictions, events_test)
#'
#' # Plot predictions for one deployment
#' deployid <- deployments(lunge_sensors)[1]
#' sb_plot_predictions(deployid, lunge_sensors, predictions, outcomes)
#'
#' @name sb_plot
NULL

#' @rdname sb_plot
#' @export
sb_plot_data <- function(deployid, sensors, events) {
  stopifnot(inherits(sensors, "Sensors"),
            inherits(events, "Events"),
            deployid %in% deployments(sensors),
            deployid %in% deployments(events))

  plotly_json <- deployid %>%
    .sbenv$sb_viz$plot_sensors_events(sensors@.data, events@.data) %>%
    .sbenv$util$plotly_to_json()

  plotly_json %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    plotly::as_widget()
}

#' @rdname sb_plot
#' @export
sb_plot_predictions <- function(deployid,
                                sensors,
                                predictions,
                                outcomes = NULL) {
  stopifnot(inherits(sensors, "Sensors"),
            inherits(predictions, "Predictions"),
            deployid %in% deployments(sensors),
            deployid %in% deployments(predictions))
  if (!is.null(outcomes)) {
    stopifnot(inherits(outcomes, "Outcomes"),
              deployid %in% deployments(outcomes))
  }

  plotly_json <- deployid %>%
    .sbenv$sb_viz$plot_predictions(sensors@.data,
                                   predictions@.data,
                                   outcomes@.data) %>%
    .sbenv$util$plotly_to_json()

  plotly_json %>%
    jsonlite::fromJSON(simplifyVector = FALSE) %>%
    plotly::as_widget()
}
