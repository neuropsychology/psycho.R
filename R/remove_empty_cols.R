#' Remove empty columns..
#'
#' Removes all columns containing ony NaNs.
#'
#' @param df Dataframe.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
remove_empty_cols <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
  return(df)
}
