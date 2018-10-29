#' RMSEA interpreation.
#'
#' Interpret RMSEA with a set of rules.
#'
#' @param x RMSEA.
#' @param rules Can be "awang2012", or a custom list.
#'
#' @examples
#' library(psycho)
#' interpret_RMSEA(0.04)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_RMSEA <- function(x, rules = "awang2012") {
  interpretation <- sapply(x, .interpret_RMSEA, rules = rules, return_rules = FALSE)
  return(interpretation)
}





#' @keywords internal
.interpret_RMSEA <- function(x, rules = "awang2012", return_rules = TRUE) {
  if (!is.list(rules)) {
    if (rules == "awang2012") {
      rules <- list(
        "good" = 0,
        "acceptable" = 0.05,
        "poor" = 0.08
      )
    } else {
      stop("rules must be either a list or 'awang2012'.")
    }
  }

  x <- (abs(x) - unlist(rules))
  s <- names(which.min(x[x >= 0]))
  if (is.null(s)) {
    s <- NA
  }

  if (return_rules) {
    return(list(interpretation = s, rules = rules))
  } else {
    return(s)
  }
}
