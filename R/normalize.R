#' Normalize (scale and reduce) numeric variables.
#'
#' Select numeric variables and normalize (Z-score) them.
#'
#' @param df Dataframe.
#' @param except Character or list of characters of column names to be excluded from normalization.
#'
#' @return Dataframe.
#'
#' @examples
#' df <- data.frame(
#'   Participant = as.factor(rep(1:50,each=2)),
#'   Condition = base::rep_len(c("A", "B"), 100),
#'   V1 = rnorm(100, 30, .2),
#'   V2 = runif(100, 3, 5),
#'   V3 = rnorm(100, 100, 10)
#'   )
#'
#' dfZ <- normalize(df)
#' dfZ <- normalize(df, except="V3")
#' dfZ <- normalize(df, except=c("V1", "V2"))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @import purrr
#' @import dplyr
#' @export
normalize <- function(df, except=NULL) {

  # Remove exceptions
  if(!is.null(except) && except %in% names(df)){
    to_keep <- as.data.frame(df[except])
    df <- df[!names(df) %in% c(except)]
  }

  # Remove non-numerics
  dfother <- purrr::discard(df, is.numeric)
  dfnum <- purrr::keep(df, is.numeric)
  dfnum <- as.data.frame(scale(dfnum))
  # Add non-numerics
  if (is.null(ncol(dfother))) {
    df <- dfnum
  } else {
    df <- dplyr::bind_cols(dfother, dfnum)
  }

  # Add exceptions
  if(!is.null(except) && exists("to_keep")){
    df <- dplyr::bind_cols(df, to_keep)
  }

  return(df)
}
