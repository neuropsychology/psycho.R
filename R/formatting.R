#' Format digits.
#'
#' @param x A digit.
#' @param digits Number of significant digits.
#' @param null_treshold Treshold below which return 0.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_digit <- function(x, digits = 2, null_treshold = 0.001) {
  rounded <- if (abs(x) < 1) {
    signif(x, digits)
  } else {
    round(x, digits)
  }

  if (abs(rounded) < null_treshold) "0" else rounded
}




#' Tidyverse-friendly sprintf.
#'
#' @param x Values.
#' @param fmt A character vector of format strings, each of up to 8192 bytes.
#' @param ... values to be passed into fmt. Only logical, integer, real and character vectors are supported, but some coercion will be done: see the ‘Details’ section. Up to 100.
#'
#'
#' @export
format_string <- function(x, fmt, ...) {
  x <- sprintf(fmt, x, ...)
  return(x)
}
