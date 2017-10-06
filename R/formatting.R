#' Dormat digits.
#'
#' @param x A digit.
#' @param digits Number of significant digits.
#' @param null_treshold Treshold below which return 0.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_digit <- function(x, digits = 2, null_treshold = 0.001) {

  rounded <- if (abs(x) < 1)
    signif(x, digits)
  else
    round(x, digits)

  if (abs(rounded) < null_treshold) "0" else rounded
}
