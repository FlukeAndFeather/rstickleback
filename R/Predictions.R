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

#' @export
setMethod("show", "Predictions", function(object) {
  n_deploy <- length(object@.data)
  cols <- object@.data[[1]]$columns$values
  cat(is(object)[[1]], "\n",
      "  ", n_deploy, " deployments.", "\n",
      "  With columns: ", paste(cols, collapse = ", "), "\n",
      sep = ""
  )
})

#' @export
#' @rdname Predictions
setMethod("deployments", "Predictions", function(object) {
  names(object@.data)
})

#' Convert Predictions to data.frame
#'
#' @param x [Predictions]
#'
#' @return [data.frame]
#' @exportS3Method base::as.data.frame
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
