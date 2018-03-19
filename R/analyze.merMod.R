#' Analyze merMod objects.
#'
#' Analyze merMod objects.
#'
#' @param x merMod object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' require(lme4)
#' fit <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#' results <- analyze(fit)
#' summary(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import lmerTest
#' @importFrom methods slot
#' @export
analyze.merMod <- function(x, ...) {
  formula <- as.character(slot(x, "call"))[2]
  data <- slot(x, "frame")

  fit <- lmerTest::lmer(formula, data)

  output <- analyze(fit)
  return(output)
}
