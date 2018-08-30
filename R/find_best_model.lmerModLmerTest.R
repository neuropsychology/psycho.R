#' Returns the best combination of predictors for lmerTest objects.
#'
#' Returns the best combination of predictors for lmerTest objects.
#'
#' @param fit A merModLmerTest object.
#' @param interaction Include interaction term.
#' @param fixed Additional formula part to add at the beginning of
#' each formula
#' @param ... Arguments passed to or from other methods.
#'
#' @return list containing all combinations.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(lmerTest)
#'
#' data <- standardize(iris)
#' fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + Petal.Length + (1|Species), data=data)
#'
#' best <- find_best_model(fit)
#' best_formula <- best$formula
#' best$table
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats update
#' @import dplyr
#'
#' @method find_best_model lmerModLmerTest
#' @export
find_best_model.lmerModLmerTest <- function(fit, interaction=TRUE, fixed=NULL, ...) {

  # Extract infos
  combinations <- find_combinations(as.formula(get_formula(fit)), interaction = interaction, fixed = fixed)

  
  # Recreating the dataset without NA
  dataComplete <- fit@frame[complete.cases(fit@frame), ]
  
  
  # fit models
  models <- c()
  for (formula in combinations) {
    newfit <- update(fit, formula, data = dataComplete)
    models <- c(models, newfit)
  }


  # No warning messages for this part
  options(warn = -1)
  
  # Model comparison
  comparison <- as.data.frame(do.call("anova", models))
  comparison$formula <- combinations

  # Re-displaying warning messages
  options(warn = 0)
  

  # Best model by criterion
  best_aic <- dplyr::arrange_(comparison, "AIC") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_aic <- as.character(best_aic[[1]])

  best_bic <- dplyr::arrange_(comparison, "BIC") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_bic <- as.character(best_bic[[1]])

  by_criterion <- data.frame(formula = c(best_aic, best_bic), criterion = c("AIC", "BIC"))

  # Best formula
  best <- table(by_criterion$formula)
  best <- names(best[which.max(best)])

  best <- list(formula = best, by_criterion = by_criterion, table = comparison)
  return(best)
}
