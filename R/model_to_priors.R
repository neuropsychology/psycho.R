#' Model to Prior.
#'
#' Convert a Bayesian model's results to priors.
#'
#' @param fit A stanreg model.
#' @param autoscale Set autoscale.
#' @examples
#' \dontrun{
#' library(rstanarm)
#' library(psycho)
#'
#' fit <- stan_glm(Sepal.Length ~ Petal.Width, data=iris)
#' priors <- model_to_priors(fit)
#' update(fit, prior=priors$prior)
#'
#' fit <- stan_glmer(Subjective_Valence ~ Emotion_Condition + (1|Participant_ID), data=psycho::emotion)
#' priors <- model_to_priors(fit)
#'
#' fit1 <- stan_glm(Subjective_Valence ~ Emotion_Condition,
#'     data=filter(psycho::emotion, Participant_ID == "1S"))
#'
#' fit2 <- stan_glm(Subjective_Valence ~ Emotion_Condition,
#'     data=filter(psycho::emotion, Participant_ID == "1S"),
#'     prior=priors$prior, prior_intercept=priors$prior_intercept)
#' }
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import dplyr
#' @importFrom stats update
#' @importFrom rstanarm normal
#' @export
model_to_priors <- function(fit, autoscale=FALSE) {
  posteriors <- as.data.frame(fit)

  # Varnames
  varnames <- names(posteriors)
  varnames <- varnames[grepl("b\\[", varnames) == FALSE]

  fixed_effects <- names(fit$coefficients)
  fixed_effects <- fixed_effects[grepl("b\\[", fixed_effects) == FALSE]
  fixed_effects <- fixed_effects[fixed_effects != "(Intercept)"]

  # Get priors
  prior_intercept <- list()
  priors <- list()
  prior_aux <- list()
  for (prior in varnames) {
    if (prior == "(Intercept)") {
      prior_intercept$mean <- mean(posteriors[[prior]])
      prior_intercept$sd <- sd(posteriors[[prior]])
    } else if (prior %in% fixed_effects) {
      priors[[prior]] <- list()
      priors[[prior]]$mean <- mean(posteriors[[prior]])
      priors[[prior]]$sd <- sd(posteriors[[prior]])
    } else {
      prior_aux[[prior]] <- list()
      prior_aux[[prior]]$mean <- mean(posteriors[[prior]])
      prior_aux[[prior]]$sd <- sd(posteriors[[prior]])
    }
  }


  prior_intercept <- rstanarm::normal(
    prior_intercept$mean,
    prior_intercept$sd,
    autoscale = autoscale
  )
  prior <- .format_priors(priors, autoscale=autoscale)
  prior_aux <- .format_priors(prior_aux, autoscale=autoscale)

  return(list(prior_intercept = prior_intercept, prior = prior, priox_aux = prior_aux))
}


#' @keywords internal
.format_priors <- function(priors, autoscale=FALSE) {
  prior_mean <- data.frame(priors) %>%
    select(contains("mean")) %>%
    gather() %>%
    select_("value") %>%
    pull()

  prior_sd <- data.frame(priors) %>%
    select(contains("sd")) %>%
    gather() %>%
    select_("value") %>%
    pull()

  prior <- rstanarm::normal(
    prior_mean,
    prior_sd,
    autoscale = autoscale
  )
}
