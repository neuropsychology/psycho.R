#' Print the results.
#'
#' @param object A psychobject class object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
summary.psychobject <- function(object, ...){
  summary <- object$summary
  return(summary)
}
