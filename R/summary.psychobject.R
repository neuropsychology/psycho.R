#' Print the results.
#'
#' Print the results.
#'
#' @param object A psychobject class object.
#' @param round Round the ouput.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method summary psychobject
#' @export
summary.psychobject <- function(object, round=NULL, ...) {
  summary <- object$summary

  if (!is.null(round)) {
    nums <- dplyr::select_if(summary, is.numeric)
    nums <- round(nums, round)
    fact <- dplyr::select_if(summary, is.character)
    fact <- cbind(fact, dplyr::select_if(summary, is.factor))
    summary <- cbind(fact, nums)
  }

  return(summary)
}
