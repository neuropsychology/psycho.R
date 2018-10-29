#' Convert probabilities to (log)odds.
#'
#' @param probs Probabilities values in vector or dataframe.
#' @param log Compute log odds (such as in logistic models)?
#'
#' @examples
#' library(psycho)
#' probs_to_odds(0.75)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
probs_to_odds <- function(probs, log = FALSE) {

  # If vector
  if (ncol(as.matrix(probs)) == 1) {
    return(.probs_to_odds(probs, log = log))
  } else {
    warning("Provide single value or vector.")
  }
}


#' @keywords internal
.probs_to_odds <- function(probs, log = FALSE) {
  odds <- probs / (1 - probs)
  if (log == TRUE) {
    odds <- log(odds)
  }
  return(odds)
}
