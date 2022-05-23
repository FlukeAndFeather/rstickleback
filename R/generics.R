#' @importFrom methods is new show

#' @title Accessors
#'
#' @description Access deployment and other information from `Events`,
#'   `Outcomes`, `Predictions`, and `Sensors` objects.
#'
#' @param object The object to access
#'
#' @return
#'
#' ## `columns`
#'
#' A list of columns of bio-logging sensor data (e.g. pitch or overall dynamic
#' body acceleration) in `object`. Applicable to `Sensors` only.
#'
#' ## `deployments`
#'
#' A list of bio-logger deployments in `objects`.
#'
#' @export
#'
#' @examples
#' c(lunge_sensors, lunge_events) %<-% load_lunges()
#' columns(lunge_sensors)
#' deployments(lunge_events)
#'
#' @rdname accessors
setGeneric("columns", function(object) standardGeneric("columns"))

#' @export
#' @rdname accessors
setGeneric("deployments", function(object) standardGeneric("deployments"))

#' @export
#' @rdname divide
setGeneric("divide", function(object, deployids) standardGeneric("divide"))
