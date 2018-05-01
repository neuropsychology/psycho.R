#' Check if model includes random effects.
#'
#' Check if model is mixed. See the
#' documentation for your model's class:
#' \itemize{
#'  \item{\link[=is.mixed.stanreg]{is.mixed.stanreg}}
#'  }
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
is.mixed <- function(fit, ...) {
  UseMethod("is.mixed")
}
