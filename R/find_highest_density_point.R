#' Find the Highest Density Point.
#'
#' Returns the Highest Density Point.
#'
#' @param x Vector.
#' @param precision Number of points in density.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_highest_density_point <- function(x, precision = 1e+03) {
  d <- x %>%
    density(n = precision) %>%
    as.data.frame()
  y <- d$x[which.max(d$y)]
  return(y)
}
