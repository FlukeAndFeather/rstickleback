#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Multiple assignment operator
#'
#' See \code{zeallot::\link[zeallot:operator]{\%<-\%}} for details.
#'
#' @name %<-%
#' @rdname multiple-assignment
#' @keywords internal
#' @export
#' @importFrom zeallot %<-%
NULL
