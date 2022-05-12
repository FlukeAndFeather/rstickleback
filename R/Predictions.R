#' @include generics.R
NULL

#' Predictions class
#'
#' Class representation for event predictions.
#'
#' Users should not create Predictions objects directly. They're the return type
#' of sb_predict().
#'
#' @slot .data A list of tuples (Series, DatetimeIndex).
#'
#' @rdname Predictions
setClass(
  "Predictions",
  slots = c(.data = "list")
)

#' @noRd
#' @export
setMethod("show", "Predictions", function(object) {
  n_deploy <- length(object@.data)
  n_events <- sum(purrr::map_int(object@.data, ~ length(.x[[2]]$values)))
  cat(is(object)[[1]], "\n",
      "  ", n_deploy, " deployments.", "\n",
      "  With ", n_events, " predicted events.","\n",
      sep = ""
  )
})

#' @export
#' @rdname accessors
setMethod("deployments", "Predictions", function(object) {
  names(object@.data)
})

#' Convert Predictions to data.frame
#'
#' @param x Predictions
#' @param ... not used
#'
#' @return data.frame
#' @exportS3Method base::as.data.frame
#' @noRd
as.data.frame.Predictions <- function(x, ...) {
  purrr::map2_dfr(names(x@.data), x@.data, function (deployid, pred) {
    result <- data.frame(deployid = deployid) %>%
      cbind(.sbenv$util$datetimeindex_to_isoformat(pred[[1]]))
    result$datetime <- lubridate::with_tz(result$datetime, "UTC")
    pred_dt <- pred[[2]]$values
    result$predicted <- result$datetime %in% pred_dt
    result
  }) %>%
    dplyr::relocate(deployid)
}
