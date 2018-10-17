#' Compute estimated marginal means and contrasts from lm models.
#'
#' Compute estimated marginal means and contrasts from an lm model.
#'
#' @param fit An lm model.
#' @param formula A character vector (formula like format, i.e., including
#' interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param adjust P value adjustment method. Default is "tukey". Can be "holm",
#' "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
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
#' @method get_contrasts lm
#' @import dplyr
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.lm <- function(fit, formula, adjust="tukey", ...) {
  formula <- as.formula(paste0("~", formula))


  # emmeans ---------------------------------------------------------------
  means <- fit %>%
    emmeans::emmeans(formula) %>%
    as.data.frame() %>%
    dplyr::rename_(
      "Mean" = "emmean",
      "CI_lower" = "lower.CL",
      "CI_higher" = "upper.CL"
    )


  # Contrasts ---------------------------------------------------------------
  contrasts <- fit %>%
    emmeans::emmeans(formula) %>%
    graphics::pairs(adjust = adjust) %>%
    as.data.frame() %>%
    dplyr::rename_(
      "Contrast" = "contrast",
      "Difference" = "estimate"
    )

  output <- list(means = means, contrasts = contrasts)
  return(output)
}
