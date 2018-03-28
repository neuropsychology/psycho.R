#' Compute estimated marginal means and contrasts from models.
#'
#' Compute estimated marginal means and contrasts from models.
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_contrasts <- function(fit, ...) {
  UseMethod("get_contrasts")
}
