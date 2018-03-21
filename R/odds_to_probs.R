#' Convert (log)odds to probabilies.
#'
#' @param odds Odds value(s).
#' @param log Are these Log odds (such as in logistic models)?
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
odds_to_probs <- function(odds, log=TRUE) {
  if (log == TRUE) {
    odds <- exp(odds)
  }
  probs <- odds / (1 + odds)
  return(probs)
}
