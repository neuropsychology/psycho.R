#' Compute estimated marginal means and contrasts from merModLmerTest models.
#'
#' Compute estimated marginal means and contrasts from a merModLmerTest models.
#'
#' @param fit A merModLmerTest model.
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
#' require(lmerTest)
#' fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1|Salary), data=affective)
#'
#' contrasts <- get_contrasts(fit, formula="Birth_Season", adjust="tukey")
#' contrasts$means
#' contrasts$contrasts
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_contrasts merModLmerTest
#' @import coda
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.merModLmerTest <- function(fit, formula, adjust="tukey", ...) {
  formula <- as.formula(paste0("~", formula))


  # emmeans ---------------------------------------------------------------
  means <- fit %>%
    emmeans::emmeans(formula) %>%
    as.data.frame() %>%
    rename_(
      "Mean" = "emmean",
      "CI_lower" = "lower.CL",
      "CI_higher" = "upper.CL"
    )


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
