
#' Plot the results.
#'
#' @param x A psychobject class object.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
plot.psychobject <- function(x, ...) {
  plot <- x$plot
  return(plot)
}


#' Print the results.
#'
#' @param x A psychobject class object.
#' @param ... Further arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
print.psychobject <- function(x, ...) {
  text <- x$text
  cat(text, sep = "\n")
  invisible(text)
}






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
summary.psychobject <- function(object, round = NULL, ...) {
  summary <- object$summary

  if (!is.null(round)) {
    nums <- dplyr::select(summary, where(is.numeric))
    nums <- round(nums, round)
    fact <- dplyr::select(summary, where(is.character))
    fact <- cbind(fact, dplyr::select(summary, where(is.factor)))
    summary <- cbind(fact, nums)
  }

  return(summary)
}






#' Extract values as list.
#'
#' @param x A psychobject class object.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
values <- function(x) {
  values <- x$values
  return(values)
}
