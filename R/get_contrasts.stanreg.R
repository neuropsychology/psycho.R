#' Compute estimated marginal means and contrasts from stanreg models.
#'
#' Compute estimated marginal means and contrasts from a stanreg models.
#'
#' @param fit A stanreg model.
#' @param formula A character vector (formula like format, i.e., including interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param prob A numeric scalar in the interval (0,1) giving the target probability content of the intervals. The nominal probability content of the intervals is the multiple of 1/nrow(obj) nearest to prob.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return list with estimated marginal means and contrasts.
#'
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' require(rstanarm)
#' fit <- rstanarm::stan_glm(Adjusting ~ Birth_Season * Sex, data=affective)
#'
#' contrasts <- get_contrasts(fit, formula="Birth_Season * Sex")
#' contrasts$means
#'
#' contrasts <- get_contrasts(fit, formula="Birth_Season")
#' contrasts$contrasts
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_contrasts stanreg
#' @import coda
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#'
#' @export
get_contrasts.stanreg <- function(fit, formula, prob=0.9, ...) {
  formula <- as.formula(paste0("~", formula))


  # emmeans ---------------------------------------------------------------
  means_posterior <- fit %>%
    emmeans::emmeans(formula) %>%
    coda::as.mcmc() %>%
    as.matrix() %>%
    coda::as.mcmc() %>%
    as.data.frame()

  means <- data.frame()

  for (name in names(means_posterior)) {
    var <- means_posterior[[name]]

    CI_values <- hdi(var, prob = prob)
    CI_values <- c(CI_values$values$HDImin, CI_values$values$HDImax)

    var <- data.frame(
      Level = name,
      Median = median(var),
      MAD = mad(var),
      Mean = mean(var),
      SD = sd(var),
      CI_lower = CI_values[seq(1, length(CI_values), 2)],
      CI_higher = CI_values[seq(2, length(CI_values), 2)]
    )

    means <- rbind(means, var)
  }


  # Contrasts ---------------------------------------------------------------
  contrasts_posterior <- fit %>%
    emmeans::emmeans(formula) %>%
    graphics::pairs() %>%
    coda::as.mcmc() %>%
    as.matrix() %>%
    coda::as.mcmc() %>%
    as.data.frame()

  contrasts <- data.frame()

  for (name in names(contrasts_posterior)) {
    var <- contrasts_posterior[[name]]

    CI_values <- hdi(var, prob = prob)
    CI_values <- c(CI_values$values$HDImin, CI_values$values$HDImax)

    var <- data.frame(
      Contrast = stringr::str_remove(name, "contrast "),
      Median = median(var),
      MAD = mad(var),
      Mean = mean(var),
      SD = sd(var),
      CI_lower = CI_values[seq(1, length(CI_values), 2)],
      CI_higher = CI_values[seq(2, length(CI_values), 2)],
      MPE = mpe(var)$MPE
    )

    contrasts <- rbind(contrasts, var)
  }


  output <- list(means = means, contrasts = contrasts)
  return(output)
}
