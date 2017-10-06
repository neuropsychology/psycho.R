#' Print the results.
#'
#' @param x A psychobject class object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
print.psychobject <- function(x, ...){
  text <- x$text
  return(text)
}
