#' @include generics.R
NULL

#' Outcomes class
#'
#' Class representation for prediction outcomes.
#'
#' Users should not create Outcomes objects directly. They're the return type
#' of sb_assess().
#'
#' @slot .data A list of Series.
#'
#' @rdname Outcomes
setClass(
  "Outcomes",
  slots = c(.data = "list")
)

#' @rdname show
#' @export
setMethod("show", "Outcomes", function(object) {
  n_deploy <- length(object@.data)
  cat(is(object)[[1]], "\n")
  as.data.frame(object) %>%
    dplyr::group_by(deployid) %>%
    dplyr::summarize(TP = sum(outcome == "TP"),
                     FP = sum(outcome == "FP"),
                     FN = sum(outcome == "FN")) %>%
    print()
})

#' @export
#' @rdname accessors
setMethod("deployments", "Outcomes", function(object) {
  names(object@.data)
})

#' Convert Outcomes to data.frame
#'
#' @param x Outcomes
#' @param ... not used
#'
#' @return data.frame
#' @exportS3Method base::as.data.frame
as.data.frame.Outcomes <- function(x, ...) {
  purrr::map2_dfr(names(x@.data), x@.data, function (deployid, out) {
    outcome <- out$values
    if (out$dtype$name == "string") outcome <- outcome$astype("str")
    result <- data.frame(deployid = deployid,
                         datetime = out$index$values,
                         outcome = outcome)
    result$datetime <- lubridate::force_tz(result$datetime, "UTC")
    result
  }) %>%
    dplyr::relocate(deployid)
}
