#' Compute estimated marginal means and contrasts from lmerMod models.
#'
#' Compute estimated marginal means and contrasts from a lmerMod models.
#'
#' @param fit A lmerMod model.
#' @param formula A character vector (formula like format, i.e., including
#' interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param adjust P value adjustment method. Default is "tukey". Can be "holm",
#' "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return list with estimated marginal means (95% CI) and contrasts.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_contrasts lmerMod
#' @import coda
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.lmerMod <- function(fit, formula, adjust="tukey", ...) {
  warning("lme4::lmer models not supported. Please refit using lmerTest::lmer.")
}
