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
#' @rdname Events
setMethod("deployments", "Events", function(object) {
  names(object@.data)
})

#' Convert Events to data.frame
#'
#' @param x [Events]
#'
#' @return [data.frame]
#' @exportS3Method base::as.data.frame
as.data.frame.Events <- function(x, ...) {
  eventsdf <- purrr::map(x@.data, "values") %>%
    purrr::map2(names(.), ~ dplyr::tibble(datetime = .x, deployid = .y)) %>%
    do.call(rbind, .)
}

#' Split events by deployment IDs
#'
#' Useful for test-train splitting.
#'
#' @param object [Events]
#' @param ids Deployment IDs in one split
#'
#' @return a list of two Events objects. The first element contains the
#'   deployments with IDs in ids, the second element contains the remainder.
#' @export
setMethod("split", "Events", function(object, ids) {
  stopifnot(all(ids %in% deployments(object)))
  eventsdf <- as.data.frame(object)
  events1 <- eventsdf %>%
    dplyr::filter(deployid %in% ids) %>%
    Events("deployid", "datetime")
  events2 <- eventsdf %>%
    dplyr::filter(!deployid %in% ids) %>%
    Events("deployid", "datetime")
  list(events1, events2)
})

