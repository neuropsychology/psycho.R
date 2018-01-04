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

  # If x is an Integer
  if (x %% 1 == 0) {
    formatted <- as.character(x)
  } else {
    # If x is close to zero
    if (abs(x) < null_treshold) {
      formatted <- "0"
    } else {
      # If x is close to trailing zeros
      if (abs(x) < 1) {
        formatted <- as.character(signif(x, digits))
        # If signif cut off trailing zero, add it
        # TODO: that line of code is ugly
        if (nchar(gsub("0|-|\\.", "", formatted)) < digits) {
          formatted <- paste0(formatted, strrep("0", digits - 1))
        }
      } else {
        formatted <- format_string(round(x, digits), paste0("%.", digits, "f"))
      }
    }
  }
  return(formatted)
}







#' Tidyverse-friendly sprintf.
#'
#' @param x Values.
#' @param fmt A character vector of format strings, each of up to 8192 bytes.

#'
#' @export
format_string <- function(x, fmt, ...) {
  x <- sprintf(fmt, x, ...)
  return(x)
}
