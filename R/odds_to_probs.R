#' Convert (log)odds to probabilies.
#'
#' @param odds Odds values in vector or dataframe.
#' @param subset Character or list of characters of column names to be
#' transformed.
#' @param except Character or list of characters of column names to be excluded
#' from transformation.
#' @param log Are these Log odds (such as in logistic models)?
#'
#' @examples
#' library(psycho)
#' odds_to_probs(-1.45)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom purrr keep discard
#' @export
odds_to_probs <- function(odds, subset=NULL, except=NULL, log=TRUE) {

  # If vector
  if (ncol(as.matrix(odds)) == 1) {
    return(.odds_to_probs(odds, log=log))
  } else{
    df <- odds
  }

  # Variable order
  var_order <- names(df)

  # Keep subset
  if (!is.null(subset) && subset %in% names(df)) {
    to_keep <- as.data.frame(df[!names(df) %in% c(subset)])
    df <- df[names(df) %in% c(subset)]
  } else {
    to_keep <- NULL
  }

  # Remove exceptions
  if (!is.null(except) && except %in% names(df)) {
    if (is.null(to_keep)) {
      to_keep <- as.data.frame(df[except])
    } else {
      to_keep <- cbind(to_keep, as.data.frame(df[except]))
    }

    df <- df[!names(df) %in% c(except)]
  }

  # Remove non-numerics
  dfother <- purrr::discard(df, is.numeric)
  dfnum <- purrr::keep(df, is.numeric)

  # Tranform
  dfnum <- .odds_to_probs(dfnum, log=log)

  # Add non-numerics
  if (is.null(ncol(dfother))) {
    df <- dfnum
  } else {
    df <- dplyr::bind_cols(dfother, dfnum)
  }

  # Add exceptions
  if (!is.null(subset) | !is.null(except) && exists("to_keep")) {
    df <- dplyr::bind_cols(df, to_keep)
  }

  # Reorder
  df <- df[var_order]

  return(df)
}


#' @keywords internal
.odds_to_probs <- function(odds, log=TRUE){
  if (log == TRUE) {
    odds <- exp(odds)
  }
  probs <- odds / (1 + odds)
return(probs)
}
