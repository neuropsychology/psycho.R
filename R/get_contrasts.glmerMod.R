#' Compute estimated marginal means and contrasts from glmerMod models.
#'
#' Compute estimated marginal means and contrasts from a glmerMod model.
#'
#' @param fit A glmerMod model.
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
#' fit <- lme4::glmer(Sex ~ Birth_Season + (1|Salary), data=affective, family="binomial")
#'
#' contrasts <- get_contrasts(fit, formula="Birth_Season", adjust="tukey")
#' contrasts$means
#' contrasts$contrasts
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_contrasts glmerMod
#' @import coda
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.glmerMod <- function(fit, formula, adjust="tukey", ...) {
  formula <- as.formula(paste0("~", formula))


  # emmeans ---------------------------------------------------------------
  means <- fit %>%
    emmeans::emmeans(formula) %>%
    as.data.frame() %>%
    rename_(
      "Mean" = "emmean",
      "CI_lower" = "asymp.LCL",
      "CI_higher" = "asymp.UCL"
    )
  means$Probability <- odds_to_probs(means$Mean)


  # Contrasts ---------------------------------------------------------------
  contrasts <- fit %>%
    emmeans::emmeans(formula) %>%
    graphics::pairs(adjust = adjust) %>%
    as.data.frame() %>%
    rename_(
      "Contrast" = "contrast",
      "Difference" = "estimate"
    )

  output <- list(means = means, contrasts = contrasts)
  return(output)
}
