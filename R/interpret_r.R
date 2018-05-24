#' Return the interpretation of a correlation coefficient following Evans (1996).
#'
#' @param x Correlation coefficient.
#' @param direction Return direction.
#' @param strength Return strength.
#'
#' @return The interpretation following Evans (1996).
#'
#' @examples
#' library(psycho)
#' interpret_r(-0.42)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_r <- function(x, direction=TRUE, strength=TRUE) {
  if (abs(x) < .20) {
    s <- "very weak"
  } else if (abs(x) < .40) {
    s <- "weak"
  } else if (abs(x) < .60) {
    s <- "moderate"
  } else if (abs(x) < .80) {
    s <- "strong"
  } else {
    s <- "very strong"
  }



  if (x > 0) {
    d <- "positive"
  } else {
    d <- "negative"
  }

  if (strength & direction) {
    interpretation <- paste(s, "and", d)
  } else if (strength & direction == FALSE) {
    interpretation <- s
  } else {
    interpretation <- d
  }

  return(interpretation)
}
