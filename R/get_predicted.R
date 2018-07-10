#' Compute predicted values from models.
#'
#' Compute predicted values from models. See the
#' documentation for your model's class:
#' \itemize{
#'  \item{\link[=get_predicted.stanreg]{get_predicted.stanreg}}
#'  \item{\link[=get_predicted.lm]{get_predicted.lm}}
#'  }
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_predicted <- function(fit, ...) {
  UseMethod("get_predicted")
}
