#' Print the results.
#'
#' @param object A psycho class object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
summary.psycho <- function(object, ...){
  summary <- object$summary
  return(summary)
}
