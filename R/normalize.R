#' Normalize (scale and reduce) numeric variables.
#'
#' Select numeric variables and normalize (Z-score) them.
#'
#' @param df Dataframe.
#'
#' @return Dataframe.
#'
#' @examples
#' df <- data.frame(Participant = as.factor(rep(1:50,each=2)),
#'   Condition = base::rep_len(c("A", "B"), 100), V1 = rnorm(100, 30, .2),
#'   V2 = runif(100, 3, 5))
#'
#' dfZ <- normalize(df)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @import purrr
#' @import dplyr
#' @export
normalize <- function(df){
  dfother <- purrr::discard(df, is.numeric)
  dfnum <- purrr::keep(df, is.numeric)
  dfnum <- as.data.frame(scale(dfnum))
  if (is.null(ncol(dfother))){
    df <- dfnum
  } else{
    df <- dplyr::bind_cols(dfother, dfnum)
  }

  return(df)
}
