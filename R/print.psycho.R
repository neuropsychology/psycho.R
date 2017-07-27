#' Print the results.
#'
#' @param x A psycho class object
#' @param ... further arguments passed to or from other methods.
#'
#' @export
print.psycho <- function(x, ...){
  text <- x$text
  print(text)
}
