#' Formatting
#'
#' @param x number.
#' @param digits number of significant digits.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @examples
#'
#' format_digit(1.20)
#' format_digit(1.2)
#' format_digit(1.2012313)
#' format_digit(0.0045)
#'
#' @export
format_digit <- function(x, digits=2){
  return(trimws(format(round(x, digits), nsmall = digits)))
}
