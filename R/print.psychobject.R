#' Print the results.
#'
#' @param x A psychobject class object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
print.psychobject <- function(x, ...){
  text <- x$text
  return(text)
}
