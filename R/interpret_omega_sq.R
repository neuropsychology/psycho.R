#' Omega Squared Interpretation
#'
#' Return the interpretation of Omegas Squared.
#'
#' @param x Omega Squared.
#' @param rules Can be "field2013" (default), or a custom list.
#'
#' @examples
#' library(psycho)
#' interpret_omega_sq(x = 0.05)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
#'
#' @references
#' \itemize{
#'  \item{Field, A (2013) Discovering statistics using IBM SPSS Statistics. Fourth Edition. Sage:London.}
#'  }
#' @export
interpret_omega_sq <- function(x, rules = "field2013") {
  interpretation <- sapply(x, .interpret_omega_sq, rules = rules, return_rules = FALSE)
  return(interpretation)
}






#' @keywords internal
.interpret_omega_sq <- function(x, rules = "field2013", return_rules = TRUE) {
  if (!is.list(rules)) {
    if (rules == "field2013") {
      rules <- list(
        "very small" = 0,
        "small" = 0.01,
        "medium" = 0.06,
        "large" = 0.14
      )
    } else {
      stop("rules must be either a list or 'field2013'.")
    }
  }



  interpretation <- (abs(x) - unlist(rules))
  interpretation <- names(which.min(interpretation[interpretation >= 0]))
  if (is.null(interpretation)) {
    interpretation <- NA
  }

  return(interpretation)
}
