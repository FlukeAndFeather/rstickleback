#' Load sample data: blue whale lunge-feeding
#'
#' These sample data are provided to gain familiarity with Stickleback
#' functionality. They contain bio-logging sensor data and labeled behavioral
#' events for six feeding blue whales.
#'
#' Blue whales (*Balaenoptera musculus*) prey on krill schools using a behavior
#' called "lunge feeding". Lunges have a stereotypical kinematic signature that
#' makes them readily identifiable in bio-logging sensor data. First, the animal
#' accelerates from about 2 to 4-5 m/s, then it opens its mouth and decelerates
#' to almost a complete stop (<1 m/s). Blue whales usually lunge from below, so
#' the pitch is often positive (~30-45 deg) and sometimes they roll on their
#' side (~90 deg) or even do a barrel roll (0-360 deg). See [Cade et al.
#' (2016)](https://doi.org/10.1016/j.cub.2016.07.037), [Goldbogen et al.
#' (2006)](https://doi.org/10.1242/jeb.02135), and [Goldbogen et al.
#' (2017)](https://doi.org/10.1146/annurev-marine-122414-033905) for more
#' details.
#'
#' The bio-logging sensor data made available by `load_lunges` contains 1.5 - 2
#' hour records from six blue whales tagged on September 4th and 5th, 2018. The
#' 11.5 hours of data include 218 lunge feeding events, and were previously
#' published by [Goldbogen et al.
#' (2019)](https://doi.org/10.1126/science.aax9044).
#'
#' The examples in \code{\link{sb_plot_data}}, \code{\link{sb_fit}},
#' \code{\link{sb_predict}}, and \code{\link{sb_assess}} show how to use these
#' sample data.
#'
#' @references
#'
#' Cade, D. E., Friedlaender, A. S., Calambokidis, J., & Goldbogen, J. A.
#' (2016). Kinematic diversity in rorqual whale feeding mechanisms. *Current
#' Biology, 26*(19), 2617-2624.
#'
#' Goldbogen, J. A., Calambokidis, J., Shadwick, R. E., Oleson, E. M., McDonald,
#' M. A., & Hildebrand, J. A. (2006). Kinematics of foraging dives and
#' lunge-feeding in fin whales. *Journal of Experimental Biology, 209*(7),
#' 1231-1244.
#'
#' Goldbogen, J. A., Cade, D. E., Calambokidis, J., Friedlaender, A. S., Potvin,
#' J., Segre, P. S., & Werth, A. J. (2017). How baleen whales feed: the
#' biomechanics of engulfment and filtration. *Annual Review of Marine Science,
#' 9*, 367-386.
#'
#' Goldbogen, J. A., Cade, D. E., Wisniewska, D. M., Potvin, J., Segre, P. S.,
#' Savoca, M. S., ... & Pyenson, N. D. (2019). Why whales are big but not
#' bigger: Physiological drivers and ecological limits in the age of ocean
#' giants. *Science, 366*(6471), 1367-1372.
#'
#' @return `[list(Sensors, Events)]` A list with two named elements. `sensors`
#'   is a \code{\link{Sensors}} object with bio-logging sensor data collected
#'   from feeding blue whales and `events` is an \code{\link{Events}} object
#'   with the times of feeding events.
#'
#' @export
load_lunges <- function() {
  lunge_data <- .sbenv$sb_data$load_lunges()

  # Sample data includes pandas DataFrames with sub-second index precision.
  # reticulate fails to convert such DataFrames (see issue 1063
  # https://github.com/rstudio/reticulate/issues/1063), so use
  # datetimeindex_to_isoformat() as a workaround first, before calling
  # py_to_r().
  import_sensors <- function(df) {
    result <- .sbenv$util$datetimeindex_to_isoformat(df)
    # Revert reticulate's automatic conversion to local timezone
    result$datetime <- lubridate::with_tz(result$datetime, "UTC")
    result
  }

  # Workaround for issue 27
  import_events <- function(dt) {
    result <- .sbenv$util$datetimeindex_values(dt)
    dim(result) <- NULL
    result
  }

  lunge_sensors <- purrr::map(lunge_data[[1]], import_sensors) %>%
    purrr::map2_dfr(names(.), ~ dplyr::mutate(.x, deployid = .y)) %>%
    Sensors("deployid", "datetime", c("depth", "pitch", "roll", "speed"))

  lunge_events <- purrr::map(lunge_data[[2]], import_events) %>%
    purrr::map2_dfr(names(.), ~ data.frame(datetime = .x, deployid = .y)) %>%
    Events("deployid", "datetime")

  list(sensors = lunge_sensors,
       events = lunge_events)
}
