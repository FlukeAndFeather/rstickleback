#' Load sample data: blue whale lunge-feeding
#'
#' @return A list with elements `sensors` (list of data frames containing sensor
#'   data, named by deployment ID) and `events` (list of labeled events, also
#'   named by deployment ID).
#' @export
#'
#' @examples
#' sample_data <- load_lunges()
load_lunges <- function() {
  lunge_data <- .sbenv$sb_data$load_lunges()

  # Sample data includes pandas DataFrames with sub-second index precision.
  # reticulate fails to convert such DataFrames (see issue 1063
  # https://github.com/rstudio/reticulate/issues/1063), so use
  # datetimeindex_to_isoformat() as a workaround first, before calling
  # py_to_r().
  import_sensors <- function(df) {
    result <- df %>%
      .sbenv$util$datetimeindex_to_isoformat()
    # Revert conversion to local timezone
    result$datetime <- lubridate::with_tz(result$datetime, "UTC")
    result
  }
  lunge_sensors <- purrr::map(lunge_data[[1]], import_sensors) %>%
    purrr::map2(names(.), ~ dplyr::mutate(.x, deployid = .y)) %>%
    dplyr::bind_rows() %>%
    Sensors("deployid", "datetime", c("depth", "pitch", "roll", "speed"))

  lunge_events <- purrr::map(lunge_data[[2]], "values") %>%
    purrr::map2(names(.), ~ dplyr::tibble(datetime = .x, deployid = .y)) %>%
    do.call(rbind, .) %>%
    Events("deployid", "datetime")

  list(sensors = lunge_sensors,
       events = lunge_events)
}

#' Load a pre-fitted Stickleback model and associated data.
#'
#' For internal use only. See also fitted_model.R.
#'
#' @return List with elements:
#'   * $stickleback [Stickleback] fitted Stickleback object
#'   * $sensors_train [Sensors] sensor data used for training
#'   * $events_train [Events] events data used for training
#'   * $sensors_test [Sensors] sensor data used for testing
#'   * $events_test [Events] events data used for testing
#'   * $predictions [Predictions] predicted events
#'   * $outcomes [Outcomes] prediction outcomes
#' @noRd
load_fitted <- function(data_loc) {
  # Load pickled data
  load_py <- function(pkl_name) {
    reticulate::py_load_object(file.path(data_loc, pkl_name))
  }
  sb_py <- load_py("prefitted.pkl")
  tsc_py <- load_py("prefitted_tsc.pkl")
  sensors_train_py <- load_py("prefitted_trainX.pkl")
  events_train_py <- load_py("prefitted_trainy.pkl")
  sensors_test_py <- load_py("prefitted_testX.pkl")
  events_test_py <- load_py("prefitted_testy.pkl")
  predictions_py <- load_py("prefitted_pred.pkl")
  outcomes_py <- load_py("prefitted_outcome.pkl")

  # Convert to R S4 objects
  sb <- new("Stickleback",
            local_clf = tsc_py,
            win_size = sb_py$win_size,
            tol = sb_py$tol$total_seconds(),
            .stickleback = sb_py)

  new_instance <- function(class, data_obj) { new(class, .data = data_obj) }
  sensors_train <- new_instance("Sensors", sensors_train_py)
  events_train <- new_instance("Events", events_train_py)
  sensors_test <- new_instance("Sensors", sensors_test_py)
  events_test <- new_instance("Events", events_test_py)
  predictions <- new_instance("Predictions", predictions_py)
  outcomes <- new_instance("Outcomes", outcomes_py)

  # Return
  list(stickleback = sb,
       sensors_train = sensors_train,
       events_train = events_train,
       sensors_test = sensors_test,
       events_test = events_test,
       predictions = predictions,
       outcomes = outcomes)
}
