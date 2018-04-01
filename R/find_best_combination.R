#' Returns the best combination of predictors.
#'
#' Returns the best combination of predictors.
#'
#' @param fit Model
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_best_combination <- function(fit, ...) {
  UseMethod("find_best_combination")
}
