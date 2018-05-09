#' Perfect Normal Distribution.
#'
#' Generates a sample of size n with a near-perfect normal distribution.
#'
#' @param n number of observations. If length(n) > 1, the length is taken to be the number required.
#' @param mean vector of means.
#' @param sd vector of standard deviations.
#' @param method "qnorm" or "average".
#' @param iter number of iterations (precision).
#'
#' @examples
#' library(psycho)
#' x <- rnorm_perfect(10)
#' plot(density(x))
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats rnorm
#' @export
rnorm_perfect <- function(n, mean = 0, sd = 1, method="qnorm", iter=10000) {
  if(method=="average"){
    x <- rowMeans(replicate(iter, sort(rnorm(n, mean, sd))))
  } else {
    x <- qnorm(seq(1/n, 1-1/n, length.out = n), mean, sd)
  }
  return(x)
}
