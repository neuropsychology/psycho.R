#' Analyze objects.
#'
#' Analyze objects.
#'
#' @param x object to analyze.
#' @param ... Arguments passed to or from other methods.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
analyze <- function(x, ...){
  UseMethod("analyze")
}
