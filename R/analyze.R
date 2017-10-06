#' Analyze objects.
#'
#' Analyze objects.
#'
#' @param x object to analyze.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
analyze <- function(x, ...){
  UseMethod("analyze")
}
