#' Print the results.
#'
#' @param x A psycho class object
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
print.psycho <- function(x, ...){
  text <- x$text
  return(text)
}
