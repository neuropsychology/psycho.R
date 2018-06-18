#' Analyze stanreg objects.
#'
#' Analyze stanreg objects.
#'
#' @param x A stanreg model.
#' @param CI Credible interval bounds.
#' @param effsize Compute Effect Sizes according to Cohen (1988). For linear models only.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_d]{interpret_d}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return Contains the following indices:
#' \itemize{
#'  \item{the Median of the posterior distribution of the parameter (can be used as a point estimate, similar to the beta of frequentist models).}
#'  \item{the Median Absolute Deviation (MAD), a robust measure of dispertion (could be seen as a robust version of SD).}
#'  \item{the Credible Interval (CI) (by default, the 90\% CI; see Kruschke, 2018), representing a range of possible parameter.}
#'  \item{the Maximum Probability of Effect (MPE), the probability that the effect is positive or negative (depending on the median’s direction).}
#'  \item{the Overlap (O), the percentage of overlap between the posterior distribution and a normal distribution of mean 0 and same SD than the posterior. Can be interpreted as the probability that a value from the posterior distribution comes from a null distribution.}
#'  }
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' data <- attitude
#' fit <- rstanarm::stan_glm(rating ~ advance + privileges, data=data)
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#' plot(results)
#'
#'
#' fit <- rstanarm::stan_glmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#' results <- analyze(fit)
#' summary(results)
#'
#' fit <- rstanarm::stan_glmer(Sex ~ Adjusting, data=psycho::affective, family="binomial")
#' results <- analyze(fit)
#' summary(results)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import rstanarm
#' @import loo
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @importFrom stats quantile as.formula
#' @importFrom utils head tail capture.output
#' @importFrom broom tidy
#' @importFrom stringr str_squish str_replace
#' @export
analyze.stanreg <- function(x, CI=90, effsize=TRUE, effsize_rules="cohen1988", ...) {
  fit <- x

  # Info --------------------------------------------------------------------

  # Algorithm
  if (fit$algorithm == "optimizing") {
    stop("Can't analyze models fitted with 'optimizing' algorithm.")
  }
  computations <- capture.output(fit$stanfit)
  computations <- paste0(computations[2], computations[3], collapse = "")
  computations <- stringr::str_remove_all(computations, ", total post-warmup draws.*")
  computations <- stringr::str_remove_all(computations, " draws per chain")
  computations <- stringr::str_replace_all(computations, "=", " = ")

  # Extract posterior distributions
  posteriors <- as.data.frame(fit)


  # Varnames
  predictors <- all.vars(as.formula(fit$formula))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  varnames <- names(fit$coefficients)
  varnames <- varnames[grepl("b\\[", varnames) == FALSE]

  # Initialize empty values
  values <- list(model = list(), effects = list())

  values$model$formula <- fit$formula
  values$model$outcome <- outcome
  values$model$predictors <- predictors

  # Priors
  info_priors <- rstanarm::prior_summary(fit)
  values$priors <- info_priors

  # R2 ----------------------------------------------------------------------

  R2 <- get_R2(fit, silent = TRUE)
  if (is.list(R2)) {
    posteriors$R2 <- R2$R2_posterior
    R2.adj <- R2$R2.adj
    if (!"R2" %in% varnames) {
      varnames <- c("R2", varnames)
    }
    R2 <- TRUE
  } else {
    R2 <- FALSE
  }

  # Random effect info --------------------------------------------
  if (is.mixed(fit)) {
    random_info <- broom::tidy(fit, parameters = "varying") %>%
      dplyr::rename_(
        "Median" = "estimate",
        "MAD" = "std.error"
      )
    values$random <- random_info
  }

  # Standardized posteriors --------------------------------------------
  if (fit$family$family == "gaussian") {
    posteriors_std <- standardize(fit)
  } else {
    posteriors_std <- NA
    effsize <- FALSE
  }


  # Get indices of each variable --------------------------------------------

  # Loop over all variables
  for (varname in varnames) {
    if (varname == "R2") {
      values$effects[[varname]] <- .process_R2(varname,
        posteriors,
        info_priors,
        R2.adj = R2.adj,
        CI = CI,
        effsize = effsize
      )
    } else if (varname == "(Intercept)") {
      values$effects[[varname]] <- .process_intercept(varname,
        posteriors,
        info_priors,
        predictors,
        CI = CI,
        effsize = effsize
      )
    } else {
      values$effects[[varname]] <- .process_effect(varname,
        posteriors,
        posteriors_std = posteriors_std,
        info_priors,
        predictors,
        CI = CI,
        effsize = effsize,
        effsize_rules = effsize_rules
      )
    }
  }


  # Summary --------------------------------------------------------------------
  summary <- data.frame()
  for (varname in varnames) {
    summary <- rbind(
      summary,
      data.frame(
        Variable = varname,
        Median = values$effects[[varname]]$median,
        MAD = values$effects[[varname]]$mad,
        CI_lower = values$effects[[varname]]$CI_values[1],
        CI_higher = values$effects[[varname]]$CI_values[2],
        MPE = values$effects[[varname]]$MPE,
        Overlap = values$effects[[varname]]$overlap
      )
    )
  }


  # Text --------------------------------------------------------------------
  # -------------------------------------------------------------------------
  # Model
  info <- paste0(
    "We fitted a ",
    ifelse(fit$algorithm == "sampling", "Markov Chain Monte Carlo", fit$algorithm),
    " ",
    fit$family$family,
    " (link = ",
    fit$family$link,
    ") model (",
    computations,
    ") to predict ",
    outcome,
    " (formula = ", stringr::str_squish(paste0(format(fit$formula), collapse = "")),
    "). The model's priors were set as follows: "
  )

  # Priors
  text_priors <- rstanarm::prior_summary(fit)
  if ("adjusted_scale" %in% names(text_priors$prior) & !is.null(text_priors$prior$adjusted_scale)) {
    scale <- paste0(
      "), scale = (",
      paste(sapply(text_priors$prior$adjusted_scale, format_digit), collapse = ", ")
    )
  } else {
    scale <- paste0(
      "), scale = (",
      paste(sapply(text_priors$prior$scale, format_digit), collapse = ", ")
    )
  }

  info_priors_text <- paste0(
    "  ~ ",
    text_priors$prior$dist,
    " (location = (",
    paste(text_priors$prior$location, collapse = ", "),
    scale,
    "))"
  )

  # Coefs
  coefs_text <- c()
  for (varname in varnames) {
    effect_text <- values$effects[[varname]]$text
    if (effsize == TRUE) {
      if (!varname %in% c("(Intercept)", "R2")) {
        effsize_text <- stringr::str_replace(
          values$effects[[varname]]$EffSize_text,
          "The effect's size",
          "It"
        )[1]
        effect_text <- paste(effect_text, effsize_text)
      }
    }
    coefs_text <- c(coefs_text, effect_text)
  }

  # Text
  if ("R2" %in% varnames) {
    text <- c(
      info,
      "",
      info_priors_text,
      "",
      "",
      paste0(
        tail(coefs_text, 1),
        head(coefs_text, 1)
      ),
      "",
      head(tail(coefs_text, -1), -1)
    )
  } else {
    text <- c(
      info,
      "",
      info_priors_text,
      "",
      "",
      head(coefs_text, 1),
      "",
      tail(coefs_text, 1)
    )
  }




  # Plot --------------------------------------------------------------------
  # -------------------------------------------------------------------------

  plot <- posteriors[varnames] %>%
    # select(-`(Intercept)`) %>%
    gather() %>%
    rename_(Variable = "key", Coefficient = "value") %>%
    ggplot(aes_string(x = "Variable", y = "Coefficient", fill = "Variable")) +
    geom_violin() +
    geom_boxplot(fill = "grey", alpha = 0.3, outlier.shape = NA) +
    stat_summary(
      fun.y = "mean", geom = "errorbar",
      aes_string(ymax = "..y..", ymin = "..y.."),
      width = .75, linetype = "dashed", colour = "red"
    ) +
    geom_hline(aes(yintercept = 0)) +
    theme_classic() +
    coord_flip() +
    scale_fill_brewer(palette = "Set1") +
    scale_colour_brewer(palette = "Set1")



  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}




















#' @keywords internal
.get_info_priors <- function(varname, info_priors, predictors=NULL) {
  # Prior
  # TBD: this doesn't work with categorical predictors :(
  values <- list()

  if (varname == "(Intercept)") {
    values["prior_distribution"] <- info_priors$prior_intercept$dist
    values["prior_location"] <- info_priors$prior_intercept$location
    values["prior_scale"] <- info_priors$prior_intercept$scale
    values["prior_adjusted_scale"] <- info_priors$prior_intercept$adjusted_scale
  } else {
    if (varname %in% predictors) {
      predictor_index <- which(predictors == varname)
      if (length(info_priors$prior$dist) == 1) {
        info_priors$prior$dist <- rep(
          info_priors$prior$dist,
          length(info_priors$prior$location)
        )
      }
      values["prior_distribution"] <- info_priors$prior$dist[predictor_index]
      values["prior_location"] <- info_priors$prior$location[predictor_index]
      values["prior_scale"] <- info_priors$prior$scale[predictor_index]
      values["prior_adjusted_scale"] <- info_priors$prior$adjusted_scale[predictor_index]
    }
  }
  return(values)
}








#' @keywords internal
.process_R2 <- function(varname, posteriors, info_priors, R2.adj=NULL, CI=90, effsize=FALSE) {
  values <- .get_info_priors(varname, info_priors)
  posterior <- posteriors[, varname]

  # Find basic posterior indices
  values$posterior <- posterior
  values$median <- median(posterior)
  values$mad <- mad(posterior)
  values$mean <- mean(posterior)
  values$sd <- sd(posterior)
  values$CI_values <- hdi(posterior, prob = CI / 100)
  values$CI_values <- c(values$CI_values$values$HDImin, values$CI_values$values$HDImax)
  values$MPE <- NA
  values$MPE_values <- NA
  values$overlap <- NA
  values$adjusted_r_squared <- R2.adj

  # Text
  values$text <- paste0(
    "The model explains about ",
    format_digit(values$median * 100),
    "% of the outcome's variance (MAD = ",
    format_digit(values$mad),
    ", ",
    CI,
    "% CI [",
    format_digit(values$CI_values[1], null_treshold = 0.0001),
    ", ",
    format_digit(values$CI_values[2], null_treshold = 0.0001),
    "]"
  )

  if (is.null(R2.adj) | is.na(R2.adj)) {
    values$text <- paste0(
      values$text,
      ")."
    )
  } else {
    values$text <- paste0(
      values$text,
      ", adj. R2 = ",
      format_digit(R2.adj),
      ")."
    )
  }


  # Effize
  if (effsize == TRUE) {
    values$std_posterior <- NA
    values$std_median <- NA
    values$std_mad <- NA
    values$std_mean <- NA
    values$std_sd <- NA
    values$std_CI_values <- NA
    values$std_CI_values <- NA

    values$EffSize <- NA
    values$EffSize_text <- NA
    values$EffSize_VeryLarge <- NA
    values$EffSize_Large <- NA
    values$EffSize_Moderate <- NA
    values$EffSize_Small <- NA
    values$EffSize_VerySmall <- NA
    values$EffSize_Opposite <- NA
  }

  return(values)
}




#' @keywords internal
.process_intercept <- function(varname, posteriors, info_priors, predictors, CI=90, effsize=FALSE) {
  values <- .get_info_priors(varname, info_priors, predictors)
  posterior <- posteriors[, varname]

  # Find basic posterior indices
  values$posterior <- posterior
  values$median <- median(posterior)
  values$mad <- mad(posterior)
  values$mean <- mean(posterior)
  values$sd <- sd(posterior)
  values$CI_values <- hdi(posterior, prob = CI / 100)
  values$CI_values <- c(values$CI_values$values$HDImin, values$CI_values$values$HDImax)
  values$MPE <- NA
  values$MPE_values <- NA
  values$overlap <- NA



  # Text
  values$text <- paste0(
    " The intercept is at ",
    format_digit(values$median),
    " (MAD = ",
    format_digit(values$mad),
    ", ",
    CI,
    "% CI [",
    format_digit(values$CI_values[1], null_treshold = 0.0001),
    ", ",
    format_digit(values$CI_values[2], null_treshold = 0.0001),
    "]). Within this model:"
  )

  # Effize
  if (effsize == TRUE) {
    values$std_posterior <- NA
    values$std_median <- NA
    values$std_mad <- NA
    values$std_mean <- NA
    values$std_sd <- NA
    values$std_CI_values <- NA
    values$std_CI_values <- NA

    values$EffSize <- NA
    values$EffSize_text <- NA
    values$EffSize_VeryLarge <- NA
    values$EffSize_Large <- NA
    values$EffSize_Moderate <- NA
    values$EffSize_Small <- NA
    values$EffSize_VerySmall <- NA
    values$EffSize_Opposite <- NA
  }

  return(values)
}




#' @keywords internal
.process_effect <- function(varname, posteriors, posteriors_std, info_priors, predictors, CI=90, effsize=FALSE, effsize_rules=FALSE) {
  values <- .get_info_priors(varname, info_priors, predictors)
  posterior <- posteriors[, varname]


  # Find basic posterior indices
  values$posterior <- posterior
  values$median <- median(posterior)
  values$mad <- mad(posterior)
  values$mean <- mean(posterior)
  values$sd <- sd(posterior)
  values$CI_values <- hdi(posterior, prob = CI / 100)
  values$CI_values <- c(values$CI_values$values$HDImin, values$CI_values$values$HDImax)
  values$MPE <- mpe(posterior)$MPE
  values$MPE_values <- mpe(posterior)$values
  values$overlap <- 100 * overlap(
    posterior,
    rnorm_perfect(
      length(posterior),
      0,
      sd(posterior)
    )
  )

  # Text
  if (grepl(":", varname)) {
    splitted <- strsplit(varname, ":")[[1]]
    if (length(splitted) == 2) {
      name <- paste0(
        "interaction between ",
        splitted[1], " and ", splitted[2]
      )
    } else {
      name <- varname
    }
  } else {
    name <- paste0("effect of ", varname)
  }

  direction <- ifelse(values$median > 0, "positive", "negative")

  values$text <- paste0(
    "  - The ",
    name,
    " has a probability of ",
    format_digit(values$MPE),
    "% of being ",
    direction,
    " (Median = ",
    format_digit(values$median, null_treshold = 0.0001),
    ", MAD = ",
    format_digit(values$mad),
    ", ",
    CI,
    "% CI [",
    format_digit(values$CI_values[1], null_treshold = 0.0001), ", ",
    format_digit(values$CI_values[2], null_treshold = 0.0001), "], ",
    "O = ",
    format_digit(values$overlap),
    "%)."
  )



  # Effize
  if (effsize == TRUE) {
    posterior_std <- posteriors_std[, varname]
    values$std_posterior <- posterior_std
    values$std_median <- median(posterior_std)
    values$std_mad <- mad(posterior_std)
    values$std_mean <- mean(posterior_std)
    values$std_sd <- sd(posterior_std)
    values$std_CI_values <- hdi(posterior_std, prob = CI / 100)
    values$std_CI_values <- c(values$std_CI_values$values$HDImin, values$std_CI_values$values$HDImax)


    EffSize <- interpret_d_posterior(posterior_std, rules = effsize_rules)

    EffSize_table <- EffSize$summary
    EffSize_table$Variable <- varname

    values$EffSize <- EffSize_table
    values$EffSize_text <- EffSize$text
    values$EffSize_VeryLarge <- EffSize$values$`very large`
    values$EffSize_Large <- EffSize$values$large
    values$EffSize_Moderate <- EffSize$values$moderate
    values$EffSize_Small <- EffSize$values$small
    values$EffSize_VerySmall <- EffSize$values$`very small`
    values$EffSize_Opposite <- EffSize$values$opposite
  }

  return(values)
}
