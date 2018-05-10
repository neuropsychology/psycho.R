#' Transform z score to percentile.
#'
#' @param z_score Z score.
#'
#' @examples
#' library(psycho)
#' percentile(-1.96)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats pnorm
#' @export
percentile <- function(z_score) {
  perc <- pnorm(z_score) * 100
  return(perc)
}



#' Transform a percentile to a z score.
#'
#' @param percentile Percentile
#'
#' @examples
#' library(psycho)
#' percentile_to_z(95)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats pnorm
#' @export
percentile_to_z <- function(percentile) {
  z <- qnorm(percentile / 100)
  return(z)
}
