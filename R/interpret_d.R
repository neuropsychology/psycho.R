#' Return the interpretation of a Cohen's d or a standardized coefficient following Cohen (1988).
#'
#' @param x Cohen's d value of standardized coefficient.
#' @param direction Return effect direction (positive / negative).
#' @return The interpretation according to Cohen (1988).
#'
#' @examples
#' library(psycho)
#' interpret_d(-0.42)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d <- function(x, direction=FALSE) {
  s <- ifelse(abs(x) > 1.3, "very large",
    ifelse(abs(x) >= 0.8, "large",
      ifelse(abs(x) >= 0.5, "medium",
        ifelse(abs(x) >= 0.2, "small", "very small")
      )
    )
  )

  if (x > 0) {
    d <- "positive"
  } else {
    d <- "negative"
  }

  if (direction) {
    interpretation <- paste(s, "and", d)
  } else {
    interpretation <- s
  }

  return(interpretation)
}
