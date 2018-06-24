#' Analyze objects.
#'
#' Analyze objects. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=analyze.stanreg]{analyze.stanreg}}
#'  \item{\link[=analyze.lmerModLmerTest]{analyze.merModLmerTest}}
#'  \item{\link[=analyze.glmerMod]{analyze.glmerMod}}
#'  \item{\link[=analyze.lm]{analyze.lm}}
#'  \item{\link[=analyze.lm]{analyze.glm}}
#'  }
#'  \itemize{
#'  \item{\link[=analyze.htest]{analyze.htest}}
#'  \item{\link[=analyze.aov]{analyze.aov}}
#'  }
#' \itemize{
#'  \item{\link[=analyze.fa]{analyze.fa}}
#'  \item{\link[=analyze.fa]{analyze.lavaan}}
#'  }
#'
#' @param x object to analyze.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
analyze <- function(x, ...) {
  UseMethod("analyze")
}
