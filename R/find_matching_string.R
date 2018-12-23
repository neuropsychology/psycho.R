#' Fuzzy string matching.
#'
#' @param x Strings.
#' @param y List of strings to be matched.
#' @param value Return value or the index of the closest string.
#' @param step Step by which decrease the distance.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#'
#' @examples
#' library(psycho)
#' find_matching_string("Hwo rea ouy", c("How are you", "Not this word", "Nice to meet you"))
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_matching_string <- function(x, y, value = TRUE, step = 0.1, ignore.case = TRUE) {
  z <- c()
  for (i in seq_len(length(x))) {
    s <- x[i]
    distance <- 0.99
    closest <- agrep(s, y, max.distance = distance, value = value, ignore.case = ignore.case)

    while (length(closest) != 1) {
      closest <- agrep(s, closest, max.distance = distance, value = value, ignore.case = ignore.case)
      distance <- distance - step
      if (distance < 0) {
        warning(paste0("Couldn't find matching string for '", s, "'. Try lowering the step parameter."))
        closest <- s
      }
    }
    z <- c(z, closest)
  }
  return(z)
}
