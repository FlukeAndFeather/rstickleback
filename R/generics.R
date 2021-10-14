#' @importFrom methods is new show

#' @title Accessors
#'
#' @description Access information from Events, Outcomes, Predictions, and
#'   Sensors objects.
#'
#' @param object The object to access
#'
#' @return \itemize{ \item `columns()`: a list of columns in sensor data
#'   (Sensors only). \item `deployments()`: a list of deployment IDs. }
#' @export
#' @rdname accessors
setGeneric("columns", function(object) standardGeneric("columns"))

#' @export
#' @rdname accessors
setGeneric("deployments", function(object) standardGeneric("deployments"))

#' @export
#' @rdname divide
setGeneric("divide", function(object, deployids) standardGeneric("divide"))
