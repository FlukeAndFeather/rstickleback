#' @include generics.R
NULL

#' Events class
#'
#' Class representation for labeled behavioral events in bio-logging data.
#'
#' Create an Events object with `Events`().
#'
#' @slot .data A list of Pandas DatetimeIndexes.
#'
#' @rdname Events
setClass(
  "Events",
  slots = c(.data = "list")
)

#' @param event_data `[data.frame]`. A two-column data frame with bio-logger
#'   deployment IDs in one column and event datetimes in the other. The event
#'   datetimes must be `POSIXct`.
#' @param deployid_col `[character(1)]`. The name of the column in `event_data`
#'   containing deployment IDs.
#' @param datetime_col `[character(1)]`. The name of the column in `event_data`
#'   with event datetimes.
#'
#' @examples
#' # Pretend we have behavioral event data for two dogs that like to bark at a
#' # clock tower
#' barking_df <- data.frame(
#'   dogs = rep(c("A", "B"), each = 4),
#'   barktime = as.POSIXct("2000-01-01 06:00") + lubridate::hours(0:7)
#' )
#' barking_df
#' Events(barking_df, "dogs", "barktime")
#'
#' @export
#' @rdname Events
Events <- function(event_data, deployid_col, datetime_col) {
  stopifnot(inherits(event_data, "data.frame"))
  stopifnot(is.character(deployid_col),
            length(deployid_col) == 1,
            deployid_col %in% colnames(event_data))
  stopifnot(is.character(datetime_col),
            length(datetime_col) == 1,
            datetime_col %in% colnames(event_data),
            inherits(event_data[[datetime_col]], "POSIXct"))

  new("Events",
      .data = .sbenv$util$convert_events(event_data,
                                         deployid_col,
                                         datetime_col))
}

#' Print an object
#'
#' @param object Events, Outcomes, Predictions, or Sensors
#'
#' @noRd
#' @export
setMethod("show", "Events", function(object) {
  n_deploy <- length(object@.data)
  n_events <- sum(purrr::map_int(object@.data, ~ length(.x$values)))
  cat(is(object)[[1]], "\n",
      "  ", n_deploy, " deployments.", "\n",
      "  With ", n_events, " events.","\n",
      sep = ""
  )
})

#' @export
#' @rdname accessors
setMethod("deployments", "Events", function(object) {
  names(object@.data)
})

#' Divide Events/Sensors objects by deployment IDs
#'
#' Useful for dividing bio-logging sensor data (`Sensors`) and behavioral event
#' data (`Events`) into test-train sets.
#'
#' @param object `[Events/Sensors]` An \code{\link{Events}} or
#'   \code{\link{Sensors}} object.
#' @param deployids `[character]` The deployment IDs to divide on. Each ID must
#'   be a deployment in `object`. Deployments with IDs in `deployids` will end
#'   up in one `Events/Sensors` object, the remaining deployments will be in
#'   another.
#'
#' @return `[list(Events/Sensors)]` A list of two `Events`/`Sensors` objects.
#'   The first list element contains the deployments with IDs in `deployids`,
#'   the second element contains the remainder.
#'
#' @examples
#' c(lunge_sensors, lunge_events) %<-% load_lunges()
#' divided_sensors <- divide(lunge_sensors, "bw180905-53")
#' # A Sensors object with *only* deployment "bw180905-53"
#' deployments(divided_sensors[[1]])
#' # A Sensors object with the remaining five deployments
#' deployments(divided_sensors[[2]])
#'
#' @export
#' @rdname divide
setMethod("divide", "Events", function(object, deployids) {
  stopifnot(all(deployids %in% deployments(object)))
  eventsdf <- as.data.frame(object)
  events1 <- eventsdf %>%
    dplyr::filter(deployid %in% deployids) %>%
    Events("deployid", "datetime")
  events2 <- eventsdf %>%
    dplyr::filter(!deployid %in% deployids) %>%
    Events("deployid", "datetime")
  list(events1, events2)
})

#' Convert Events to data.frame
#'
#' @param x Events
#' @param ... not used
#'
#' @return data.frame
#' @exportS3Method base::as.data.frame
#' @noRd
as.data.frame.Events <- function(x, ...) {
  purrr::map2_dfr(names(x@.data), x@.data, function(deployid, datetimeindex) {
    # Removing dimension fixes array issue with POSIXct and bind_rows
    # See https://github.com/tidyverse/dplyr/issues/5525
    # See https://github.com/r-lib/vctrs/issues/1290
    datetime <- datetimeindex$values
    dim(datetime) <- NULL
    data.frame(deployid, datetime)
  })
}
