#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts between factor levels based on a fitted model.
#' See the documentation for your model's class:
#' \itemize{
#'  \item{\link[=get_contrasts.glm]{get_contrasts.glm}}
#'  \item{\link[=get_contrasts.lmerModLmerTest]{get_contrasts.merModLmerTest}}
#'  \item{\link[=get_contrasts.glmerMod]{get_contrasts.glmerMod}}
#'  \item{\link[=get_contrasts.stanreg]{get_contrasts.stanreg}}
#'  }
#'
#'
#' @param fit A model.
#' @param ... Arguments passed to or from other methods.
#'
#' @return Estimated contrasts.
#'
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' require(lmerTest)
#' require(rstanarm)
#' 
#' fit <- lm(Adjusting ~ Birth_Season * Salary, data = affective)
#' get_contrasts(fit)
#' 
#' fit <- lm(Adjusting ~ Birth_Season * Salary, data = affective)
#' get_contrasts(fit, adjust = "bonf")
#' 
#' fit <- lmerTest::lmer(Adjusting ~ Birth_Season * Salary + (1 | Salary), data = affective)
#' get_contrasts(fit, formula = "Birth_Season")
#' 
#' fit <- rstanarm::stan_glm(Adjusting ~ Birth_Season, data = affective)
#' get_contrasts(fit, formula = "Birth_Season", ROPE_bounds = c(-0.1, 0.1))
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
get_contrasts <- function(fit, ...) {
  UseMethod("get_contrasts")
}


#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @param fit A Bayesian model.
#' @param formula A character vector (formula like format, i.e., including
#' interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param CI Determine the confidence or credible interval bounds.
#' @param ROPE_bounds Optional bounds of the ROPE for Bayesian models.
#' @param overlap Set to TRUE to add Overlap index (for Bayesian models).
#' @param ... Arguments passed to or from other methods.
#' @method get_contrasts stanreg
#' @export
get_contrasts.stanreg <- function(fit, formula = NULL, CI = 90, ROPE_bounds = NULL, overlap = FALSE, ...) {
  .get_contrasts_bayes(fit, formula, CI, ROPE_bounds, overlap, ...)
}


#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @param fit A frequentist model.
#' @param formula A character vector (formula like format, i.e., including
#' interactions or nesting terms) specifying the names of the predictors over which EMMs are desired.
#' @param CI Determine the confidence or credible interval bounds.
#' @param adjust P value adjustment method for frequentist models. Default is "tukey". Can be "holm",
#' "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr" or "none".
#' @param ... Arguments passed to or from other methods.
#' @method get_contrasts lm
#' @export
get_contrasts.lm <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  .get_contrasts_freq(fit, formula, CI, adjust, ...)
}

#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @inheritParams get_contrasts.lm
#' @method get_contrasts glm
#' @export
get_contrasts.glm <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  .get_contrasts_freq(fit, formula, CI, adjust, ...)
}

#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @inheritParams get_contrasts.lm
#' @method get_contrasts lmerModLmerTest
#' @export
get_contrasts.lmerModLmerTest <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  .get_contrasts_freq(fit, formula, CI, adjust, ...)
}

#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @inheritParams get_contrasts.lm
#' @method get_contrasts glmerMod
#' @export
get_contrasts.glmerMod <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  .get_contrasts_freq(fit, formula, CI, adjust, ...)
}

#' Compute estimated contrasts from models.
#'
#' Compute estimated contrasts from models.
#'
#' @inheritParams get_contrasts.lm
#' @method get_contrasts lmerMod
#' @export
get_contrasts.lmerMod <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  .get_contrasts_freq(fit, formula, CI, adjust, ...)
}




#' @import dplyr
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint mad
#' @keywords internal
.get_contrasts_bayes <- function(fit, formula = NULL, CI = 90, ROPE_bounds = NULL, overlap = FALSE, ...) {
  if (is.null(formula)) {
    formula <- paste(get_info(fit)$predictors, collapse = " * ")
  }

  if (is.character(formula)) {
    formula <- as.formula(paste0("~ ", formula))
  }

  # Contrasts ---------------------------------------------------------------
  contrasts_posterior <- fit %>%
    emmeans::emmeans(formula) %>%
    graphics::pairs() %>%
    emmeans::as.mcmc.emmGrid() %>%
    as.matrix() %>%
    as.data.frame()

  contrasts <- data.frame()


  for (name in names(contrasts_posterior)) {
    posterior <- contrasts_posterior[[name]]

    CI_values <- HDI(posterior, prob = CI / 100)
    CI_values <- c(CI_values$values$HDImin, CI_values$values$HDImax)

    var <- data.frame(
      Contrast = stringr::str_remove(name, "contrast "),
      Median = median(posterior),
      MAD = mad(posterior),
      CI_lower = CI_values[seq(1, length(CI_values), 2)],
      CI_higher = CI_values[seq(2, length(CI_values), 2)],
      MPE = mpe(posterior)$MPE
    )

    if (overlap == TRUE) {
      var$Overlap <- 100 * overlap(
        posterior,
        rnorm_perfect(
          length(posterior),
          0,
          sd(posterior)
        )
      )
    }

    if (!is.null(ROPE_bounds)) {
      var$ROPE <- rope(posterior, ROPE_bounds, CI = 95)$rope_probability
    }

    contrasts <- rbind(contrasts, var)
  }


  return(contrasts)
}




#' @import dplyr
#' @importFrom emmeans emmeans
#' @importFrom graphics pairs
#' @importFrom stats confint
#' @keywords internal
.get_contrasts_freq <- function(fit, formula = NULL, CI = 95, adjust = "tukey", ...) {
  if (is.null(formula)) {
    formula <- paste(get_info(fit)$predictors, collapse = " * ")
  }

  if (is.character(formula)) {
    formula <- as.formula(paste0("~ ", formula))
  }

  # Contrasts ---------------------------------------------------------------
  contrasts <- fit %>%
    emmeans::emmeans(formula) %>%
    graphics::pairs(adjust = adjust)

  # Confint
  CI <- contrasts %>%
    confint(CI / 100) %>%
    select(contains("CL"))


  contrasts <- contrasts %>%
    as.data.frame() %>%
    cbind(CI) %>%
    dplyr::rename_(
      "Contrast" = "contrast",
      "Difference" = "estimate",
      "p" = "p.value"
    )
  names(contrasts) <- stringr::str_replace(names(contrasts), "lower.CL", "CI_lower")
  names(contrasts) <- stringr::str_replace(names(contrasts), "upper.CL", "CI_higher")
  names(contrasts) <- stringr::str_replace(names(contrasts), "asymp.LCL", "CI_lower")
  names(contrasts) <- stringr::str_replace(names(contrasts), "asymp.UCL", "CI_higher")
  names(contrasts) <- stringr::str_replace(names(contrasts), "t.ratio", "t")
  names(contrasts) <- stringr::str_replace(names(contrasts), "z.ratio", "z")

  return(contrasts)
}
