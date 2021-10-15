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
            all(sensor_cols %in% colnames(sensor_data)))

  new("Sensors",
      .data = .sbenv$util$convert_sensors(sensor_data,
                                          deployid_col,
                                          datetime_col,
                                          sensor_cols))
}

#' @rdname show
#' @export
setMethod("show", "Sensors", function(object) {
  n_deploy <- length(deployments(object))
  cols <- columns(object)
  cat(is(object)[[1]], "\n",
      "  ", n_deploy, " deployments.", "\n",
      "  With column(s): ", paste(cols, collapse = ", "), "\n",
      sep = ""
  )
})

#' @export
#' @rdname accessors
setMethod("deployments", "Sensors", function(object) {
  names(object@.data)
})

#' @export
#' @rdname accessors
setMethod("columns", "Sensors", function(object) {
  if (inherits(object@.data[[1]], "pandas.core.series.Series")) {
    object@.data[[1]]$name
  } else {
    object@.data[[1]]$columns$values
  }
})

#' @export
#' @rdname divide
setMethod("divide", "Sensors", function(object, deployids) {
  stopifnot(all(deployids %in% deployments(object)))
  sensorsdf <- as.data.frame(object)
  sensors1 <- sensorsdf %>%
    dplyr::filter(deployid %in% deployids) %>%
    Sensors("deployid", "datetime", columns(object))
  sensors2 <- sensorsdf %>%
    dplyr::filter(!deployid %in% deployids) %>%
    Sensors("deployid", "datetime", columns(object))
  list(sensors1, sensors2)
})

#' Convert Sensors to data.frame
#'
#' @param x Sensors
#' @param ... not used
#'
#' @return data.frame
#' @exportS3Method base::as.data.frame
as.data.frame.Sensors <- function(x, ...) {
  import_sensors_df <- function(df, id) {
    result <- df %>%
      .sbenv$util$datetimeindex_to_isoformat()
    result$datetime <- lubridate::with_tz(result$datetime, "UTC")
    result$deployid = id
    dplyr::relocate(result, deployid)
  }
  import_sensors_series <- function(s, id) {
    dt <- s$index$values %>%
      lubridate::force_tz("UTC")
    col <- s$values
    dim(dt) <- NULL
    dim(col) <- NULL
    result <- data.frame(deployid = id, datetime = dt, col = col)
    colnames(result)[3] <- s$name
    result
  }
  f <- if (inherits(x@.data[[1]], "pandas.core.series.Series")) {
   import_sensors_series
  } else {
    import_sensors_df
  }
  purrr::map2_dfr(x@.data, names(x@.data), f)
}
