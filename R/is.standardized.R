#' Check if a dataframe is standardized.
#'
#' Check if a dataframe is standardized.
#'
#' @param df A dataframe.
#' @param tol The error treshold.
#'
#' @examples
#' library(psycho)
#'
#' df <- psycho::affective
#' is.standardized(df)
#'
#' dfZ <- psycho::standardize(df)
#' is.standardized(dfZ)
#'
#' @return bool.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import purrr
#' @export
is.standardized <- function(df, tol = 0.1) {
  dfZ <- standardize(df)
  dfZnum <- purrr::keep(dfZ, is.numeric)

  dfnum <- purrr::keep(df, is.numeric)

  error <- as.matrix(dfnum) - as.matrix(dfZnum)
  error <- as.data.frame(error)
  names(error) <- names(dfnum)

  error_mean <- error %>%
    summarise_all(mean)

  if (TRUE %in% as.character(error_mean[1, ] > tol)) {
    standardized <- FALSE
  } else {
    standardized <- TRUE
  }
  return(standardized)
}
