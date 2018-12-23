#' Find random effects in formula.
#'
#' @param formula Formula

#' @examples
#' library(psycho)
#' find_random_effects("Y ~ X + (1|Group)")
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stringr str_remove_all
#' @importFrom lme4 findbars
#' @export
find_random_effects <- function(formula) {
  random <- lme4::findbars(as.formula(formula))
  random <- paste0("(", random, ")")
  random <- stringr::str_remove_all(random, " ")
  random <- paste(random, collapse = " + ")
  return(random)
}
