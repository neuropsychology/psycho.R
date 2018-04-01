#' Generate all combinations.
#'
#' Generate all combinations.
#'
#' @param object Object
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_combinations <- function(object, ...) {
  UseMethod("find_combinations")
}
