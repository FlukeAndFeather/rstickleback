#' Plot Stickleback training data.
#'
#' Creates an interactive plotly figure for examining sensor and event data.
#'
#' @param deployid [character(1)] Deployment ID.
#' @param sensors [Sensors] Sensor data.
#' @param events [Events] Event data.
#'
#' @return A plotly figure.
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

#' Plot Stickleback predictions
#'
#' @param deployid [character(1)] Deployment ID.
#' @param sensors [Sensors] Sensor data.
#' @param predictions [Predictions]
#' @param outcomes [Outcomes]
#'
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
