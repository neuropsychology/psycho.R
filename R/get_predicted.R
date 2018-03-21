#' Compute predicted values from models.
#'
#' Compute predicted values from models.
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_predicted <- function(fit, ...) {
  UseMethod("get_predicted")
}
