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
