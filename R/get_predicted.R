#' Compute predicted values from models.
#'
#' Compute predicted values from models.
#'
#' @param x Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_predicted <- function(x, ...) {
  UseMethod("get_predicted")
}
