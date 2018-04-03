#' Returns the best model.
#'
#' Returns the best model..
#'
#' @param fit Model
#' @param ... Arguments passed to or from other methods.
#'
#' @seealso \code{\link{find_best_model.stanreg}}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_best_model <- function(fit, ...) {
  UseMethod("find_best_model")
}
