#' Illustration of Hello world
#'
#' Change something
#' prints hellow world \code{brocolors}
#' Use `devtools::document()`
#'devtools::build_vignettes()
#'
#'
#' @param method2order method to order colors (\code{"hsv"} or \code{"cluster"})
#' @param cex character expansion for the text
#' @param mar margin paramaters; vector of length 4 (see \code{\link[graphics]{par}})
#'
#' @return None
#'
#' @examples
#' hello()
#'
#' @export
hello <- function(method2order, cex, mar) {
  print("Hello, world!")
}
