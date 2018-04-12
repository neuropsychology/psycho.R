#' Get Marginal Means and Contrasts.
#'
#' Compute estimated marginal means and contrasts from models. See the
#' documentation for your model's class:
#' \itemize{
#'  \item{\link[=get_contrasts.stanreg]{get_contrasts.stanreg}}
#'  \item{\link[=get_contrasts.merModLmerTest]{get_contrasts.merModLmerTest}}
#'  \item{\link[=get_contrasts.glmerMod]{get_contrasts.glmerMod}}
#'  \item{\link[=get_contrasts.merMod]{get_contrasts.merMod}}
#'  }
#'
#'
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_contrasts <- function(fit, ...) {
  UseMethod("get_contrasts")
}
