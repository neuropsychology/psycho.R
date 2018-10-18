#' Compute estimated means from models.
#'
#' Compute estimated means of factor levels based on a fitted model.
#'
#' @param fit A model (lm, lme4 or rstanarm).
#' @param formula A character vector (formula like format, i.e., including
#' interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param CI Determine the confidence or credible interval bounds.
#' @param ... Arguments passed to or from other methods. For instance, transform="response".
#'
#'
#' @return Estimated means (or median of means for Bayesian models)
#'
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' require(lmerTest)
#' require(rstanarm)
#'
#'
#' fit <- glm(Sex ~ Birth_Season, data=affective, family="binomial")
#' get_means(fit)
#'
#' fit <- lmerTest::lmer(Adjusting ~ Birth_Season * Salary + (1|Salary), data=affective)
#' get_means(fit, formula="Birth_Season")
#'
#' fit <- rstanarm::stan_glm(Adjusting ~ Birth_Season, data=affective)
#' get_means(fit, formula="Birth_Season")
#'
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_means <- function(fit, formula=NULL, CI=90, ...) {
  UseMethod("get_means")
}


#' @method get_means stanreg
#' @export
get_means.stanreg <- function(fit, formula=NULL, CI=90, ...){
  .get_means_bayes(fit, formula, CI, ...)
}

#' @method get_means lm
#' @export
get_means.lm <- function(fit, formula=NULL, CI=95, ...){
  .get_means_freq(fit, formula, CI, ...)
}

#' @method get_means glm
#' @export
get_means.glm <- function(fit, formula=NULL, CI=95, ...){
  .get_means_freq(fit, formula, CI, ...)
}

#' @method get_means lmerModLmerTest
#' @export
get_means.lmerModLmerTest <- function(fit, formula=NULL, CI=95, ...){
  .get_means_freq(fit, formula, CI, ...)
}

#' @method get_means glmerMod
#' @export
get_means.glmerMod <- function(fit, formula=NULL, CI=95, ...){
  .get_means_freq(fit, formula, CI, ...)
}

#' @method get_means lmerMod
#' @export
get_means.lmerMod <- function(fit, formula=NULL, CI=95, ...){
  .get_means_freq(fit, formula, CI, ...)
}




#' @import dplyr
#' @importFrom emmeans emmeans
#' @importFrom stats confint mad
#' @keywords internal
.get_means_bayes <- function(fit, formula=NULL, CI=90, ...) {

  if(is.null(formula)){
    formula <- paste(get_info(fit)$predictors, collapse=" * ")
  }

  if(is.character(formula)){
    formula <- as.formula(paste0("~ ", formula))
  }

  # Means ---------------------------------------------------------------
  means_posterior <- fit %>%
    emmeans::emmeans(formula) %>%
    emmeans::as.mcmc.emmGrid() %>%
    as.matrix() %>%
    as.data.frame()

  means <- data.frame()

  for (name in names(means_posterior)) {
    var <- means_posterior[[name]]

    CI_values <- HDI(var, prob = CI/100)
    CI_values <- c(CI_values$values$HDImin, CI_values$values$HDImax)

    var <- data.frame(
      Level = name,
      Median = median(var),
      MAD = mad(var),
      CI_lower = CI_values[seq(1, length(CI_values), 2)],
      CI_higher = CI_values[seq(2, length(CI_values), 2)]
    )

    means <- rbind(means, var)
  }

  return(means)
}




#' @import dplyr
#' @importFrom emmeans emmeans
#' @importFrom stats confint
#' @keywords internal
.get_means_freq <- function(fit, formula=NULL, CI=95, ...) {

  if(is.null(formula)){
    formula <- paste(get_info(fit)$predictors, collapse=" * ")
  }

  if(is.character(formula)){
    formula <- as.formula(paste0("~ ", formula))
  }

  # Means ---------------------------------------------------------------
  means <- fit %>%
    emmeans::emmeans(formula, ...) %>%
    confint(CI/100) %>%
    as.data.frame()

  names(means) <- stringr::str_replace(names(means), "emmean", "Mean")
  names(means) <- stringr::str_replace(names(means), "lower.CL", "CI_lower")
  names(means) <- stringr::str_replace(names(means), "upper.CL", "CI_higher")
  names(means) <- stringr::str_replace(names(means), "asymp.LCL", "CI_lower")
  names(means) <- stringr::str_replace(names(means), "asymp.UCL", "CI_higher")

  return(means)
}
