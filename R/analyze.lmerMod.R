#' Analyze lmerMod objects.
#'
#' Analyze lmerMod objects.
#'
#' @param x lmerMod object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import lmerTest
#' @importFrom methods slot
#' @export
analyze.lmerMod <- function(x, ...) {
  warning("lme4::lmer models not supported. Please refit using lmerTest::lmer.")
}
