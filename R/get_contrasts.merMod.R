#' Compute estimated marginal means and contrasts from merMod models.
#'
#' Compute estimated marginal means and contrasts from a merMod models.
#'
#' @param fit A merMod model.
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
#' @examples
#' \dontrun{
#' library(psycho)
#' require(lme4)
#' fit <- lme4::lmer(Adjusting ~ Birth_Season + (1|Salary), data=affective)
#'
#' contrasts <- get_contrasts(fit, formula="Birth_Season", adjust="tukey")
#' contrasts$means
#' contrasts$contrasts
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_contrasts merMod
#' @import coda
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.merMod <- function(fit, formula, adjust="tukey", ...) {
  models_formula <- as.character(slot(fit, "call"))[2]
  data <- slot(fit, "frame")

  fit <- lmerTest::lmer(models_formula, data)

  output <- get_contrasts(fit, formula, adjust)
  return(output)
}
