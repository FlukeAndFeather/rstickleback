#' @include generics.R
NULL

#' Sensors class
#'
#' Class representation for bio-logging sensor data.
#'
#' Create a Sensors object with Sensors().
#'
#' @slot .data A list of Pandas DataFrames.
#'
#' @rdname Sensors
setClass(
  "Sensors",
  slots = c(.data = "list")
)

#' @param sensor_data a data frame. Must have columns for deployment ID and
#'   datetime (POSIXct).
#' @param deployid_col character(1). Name of column with deployment ID.
#' @param datetime_col character(1). Name of column with event datetimes.
#' @param sensor_cols character. Names of columns containing sensor data.
#'
#' @export
#' @rdname Sensors
Sensors <- function(sensor_data, deployid_col, datetime_col, sensor_cols) {
  stopifnot(inherits(sensor_data, "data.frame"))
  stopifnot(is.character(deployid_col),
            length(deployid_col) == 1,
            deployid_col %in% colnames(sensor_data))
  stopifnot(is.character(datetime_col),
            length(datetime_col) == 1,
            datetime_col %in% colnames(sensor_data),
            inherits(sensor_data[[datetime_col]], "POSIXct"))
  stopifnot(is.character(sensor_cols),
            all(deployid_col %in% colnames(sensor_data)))

  new("Sensors",
      .data = .sbenv$util$convert_sensors(sensor_data,
                                          deployid_col,
                                          datetime_col,
                                          sensor_cols))
}

#' @export
setMethod("show", "Sensors", function(object) {
  n_deploy <- length(object@.data)
  cols <- object@.data[[1]]$columns$values
  cat(is(object)[[1]], "\n",
      "  ", n_deploy, " deployments.", "\n",
      "  With columns: ", paste(cols, collapse = ", "), "\n",
      sep = ""
  )
})

#' @export
#' @rdname Sensors
setMethod("deployments", "Sensors", function(object) {
  names(object@.data)
})

#' @export
#' @rdname Sensors
setMethod("columns", "Sensors", function(object) {
  object@.data[[1]]$columns$values
})
