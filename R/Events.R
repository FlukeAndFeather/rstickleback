#' @include generics.R
NULL

#' Events class
#'
#' Class representation for labeled behavioral events in bio-logging data.
#'
#' Create an Events object with Events().
#'
#' @slot .data A list of Pandas DatetimeIndexes.
#'
#' @rdname Events
setClass(
  "Events",
  slots = c(.data = "list")
)

#' @param event_data a data frame. Must have columns for deployment ID and event
#'   datetimes (POSIXct).
#' @param deployid_col character(1). Name of column with deployment ID.
#' @param datetime_col character(1). Name of column with event datetimes.
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
#' @rdname show
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
#' Useful for dividing into test-train sets.
#'
#' @param object Events/Sensors
#' @param deployids Vector of deployment IDs
#'
#' @return a list of two Events/Sensors objects. The first element contains the
#'   deployments with IDs in deployids, the second element contains the
#'   remainder.
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
as.data.frame.Events <- function(x, ...) {
  eventsdf <- purrr::map(x@.data, "values") %>%
    purrr::map2(names(.), ~ dplyr::tibble(datetime = .x, deployid = .y)) %>%
    do.call(rbind, .)
}
