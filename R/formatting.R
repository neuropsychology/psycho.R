#' Dormat digits.
#'
#' @param x A digit.
#' @param digits Number of significant digits.
#' @param null_treshold Treshold below which return 0.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
format_digit <- function(x, digits=2, null_treshold=0.001){
  if (abs(x) < 1){
    rounded <- signif(x, digits)
  } else{
    rounded <- round(x, digits)
  }

  if (abs(rounded) < null_treshold){
    rounded <- "0"
  }
  return(rounded)
}
