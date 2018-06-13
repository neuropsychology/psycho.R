#' Omega Squared Interpretation
#'
#' Return the interpretation of Omegas Squared.
#'
#' @param x Odds ratio.
#' @param log Are these log odds ratio?
#' @param rules Can be "chen2010" (default), or a custom list.
#'
#' @examples
#' library(psycho)
#' interpret_odds(x=2)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
#'
#' @references
#' \itemize{
#'  \item{Chen, H., Cohen, P., & Chen, S. (2010). How big is a big odds ratio? Interpreting the magnitudes of odds ratios in epidemiological studies. Communications in Statistics—Simulation and Computation®, 39(4), 860-864.}
#'  }
#' @export
interpret_odds <- function(x, log=FALSE, rules="chen2010") {
  interpretation <- sapply(x, .interpret_odds, log=log, rules = rules, return_rules = FALSE)
  return(interpretation)
}


#' @keywords internal
.interpret_odds <- function(x, log=FALSE, rules="chen2010", return_rules=TRUE) {

  if(log == TRUE){
    x <- exp(abs(x))
  }

  if (!is.list(rules)) {
    if (rules == "chen2010") {
      rules <- list(
        "very small" = 0,
        "small" = 1.68,
        "medium" = 3.47,
        "large" = 6.71
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
