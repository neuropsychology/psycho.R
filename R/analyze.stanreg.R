#' Analyze stanreg objects.
#'
#' Analyze stanreg objects.
#'
#' @param x A stanreg model.
#' @param CI Credible interval bounds.
#' @param effsize Compute Effect Sizes according to Cohen (1988)? Your outcome variable must be standardized.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' data <- standardize(attitude)
#' fit <- rstanarm::stan_glm(rating ~ advance + privileges, data=data)
#'
#' results <- analyze(fit)
#' summary(results)
#' plot(results)
#' print(results)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import rstanarm
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @importFrom stats quantile
#' @importFrom utils head tail
#' @export
analyze.stanreg <- function(x, CI=90, effsize=FALSE, ...) {


  # Processing
  # -------------
  fit <- x

  predictors <- all.vars(as.formula(fit$formula))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  # Extract posterior distributions
  posteriors <- as.data.frame(fit)

  # Varnames
  varnames <- names(fit$coefficients)
  varnames <- varnames[grepl("b\\[", varnames) == FALSE]

  # If the model is an LM, extract the R2 distribution
  if ("R2" %in% names(posteriors)) {
    varnames <- c(varnames, "R2")
  } else {
    posteriors$R2 <- rstanarm::bayes_R2(fit)
    varnames <- c(varnames, "R2")
  }

  # Initialize empty values
  values <- list()
  # Loop over all variables
  for (varname in varnames) {
    # Extract posterior
    posterior <- posteriors[, varname]

    # Find basic posterior indices
    median <- median(posterior)
    mad <- mad(posterior)
    mean <- mean(posterior)
    sd <- sd(posterior)
    CI_values <- hdi(posterior, prob = CI / 100)
    CI_values <- c(CI_values$values$HDImin, CI_values$values$HDImax)

    # Compute MPE
    MPE <- mpe(posterior)$MPE
    MPE_values <- mpe(posterior)$values




    # Create text
    if (grepl(":", varname)) {
      splitted <- strsplit(varname, ":")[[1]]
      if (length(splitted) == 2) {
        name <- paste(
          "interaction effect between ",
          splitted[1], " and ", splitted[2],
          sep = ""
        )
      } else {
        name <- varname
      }
    } else {
      name <- paste("effect of ", varname, sep = "")
    }

    text <- paste0(
      "   - The ", name, " has a probability of ",
      format_digit(MPE), "% that its coefficient is between ",
      format_digit(MPE_values[1], null_treshold = 0.0001), " and ",
      format_digit(MPE_values[2], null_treshold = 0.0001),
      " (Median = ", format_digit(median, null_treshold = 0.0001),
      ", MAD = ", format_digit(mad, null_treshold = 0.0001),
      # ", Mean = ", format_digit(mean),
      # ", SD = ", format_digit(sd),
      ", ", CI, "% CI [",
      format_digit(CI_values[1], null_treshold = 0.0001), ", ",
      format_digit(CI_values[2], null_treshold = 0.0001), "], ",
      "MPE = ", format_digit(MPE), "%)."
    )

    if (varname == "(Intercept)") {
      text <- paste0(
        "The model's intercept is at ",
        format_digit(median(posterior)),
        " (MAD = ",
        format_digit(mad(posterior)),
        ", ",
        CI,
        "% CI [",
        format_digit(CI_values[1], null_treshold = 0.0001),
        ", ",
        format_digit(CI_values[2], null_treshold = 0.0001),
        "]). Within this model:"
      )
    }

    if (varname == "R2") {
      text <- paste0(
        "The model explains between ",
        format_digit(min(posterior) * 100),
        "% and ",
        format_digit(max(posterior) * 100),
        "% of the outcome's variance (R2's median = ",
        format_digit(median(posterior)),
        ", MAD = ",
        format_digit(mad(posterior)),
        ", ",
        CI,
        "% CI [",
        format_digit(CI_values[1], null_treshold = 0.0001),
        ", ",
        format_digit(CI_values[2], null_treshold = 0.0001),
        "]). "
      )
    }

    # Store all that
    values[[varname]] <- list(
      name = varname,
      median = median,
      mad = mad,
      mean = mean,
      sd = sd,
      CI_values = CI_values,
      MPE = MPE,
      MPE_values = MPE_values,
      posterior = posterior,
      text = text
    )
  }

  # Effect size
  # -------------
  if (effsize == T) {

    # Check if standardized
    model_data <- fit$data
    model_data <- model_data[all.vars(fit$formula)]
    standardized <- is.standardized(model_data)

    if (standardized == FALSE) {
      warning("It seems that your data was not standardized... Interpret effect sizes with caution!")
    }


    EffSizes <- data.frame()
    for (varname in varnames) {
      posterior <- posteriors[, varname]

      # Compute the probabilities
      mkneg <- function(pmin, pmax) {
        stopifnot(pmin < pmax) # sanity check
        length(posterior[posterior > pmin & posterior <= pmax]) /
          length(posterior)
      }

      mkpos <- function(pmin, pmax) {
        stopifnot(pmin < pmax) # sanity check
        length(posterior[posterior >= pmin & posterior < pmax]) /
          length(posterior)
      }

      verylarge_neg <- mkneg(-Inf, -1.3)
      large_neg <- mkneg(-1.3, -0.8)
      medium_neg <- mkneg(-0.8, -0.5)
      small_neg <- mkneg(-0.5, -0.2)
      verysmall_neg <- mkneg(-0.2, 0) # TODO: there was open interval at 0

      verylarge_pos <- mkpos(1.3, Inf)
      large_pos <- mkpos(0.8, 1.3)
      medium_pos <- mkpos(0.5, 0.8)
      small_pos <- mkpos(0.2, 0.5)
      verysmall_pos <- mkpos(0, 0.2) # TODO: there was open interval at 0

      EffSize <- data.frame(
        Direction = c(
          "Negative", "Negative", "Negative", "Negative",
          "Negative", "Positive", "Positive", "Positive",
          "Positive", "Positive"
        ),
        Size = c(
          "VeryLarge", "Large", "Medium", "Small", "VerySmall",
          "VerySmall", "Small", "Medium", "Large", "VeryLarge"
        ),
        Probability = c(
          verylarge_neg, large_neg, medium_neg, small_neg,
          verysmall_neg, verysmall_pos, small_pos, medium_pos,
          large_pos, verylarge_pos
        )
      )

      EffSize$Probability[is.na(EffSize$Probability)] <- 0
      EffSize$Variable <- varname

      EffSizes <- rbind(EffSizes, EffSize)

      if (mean(posterior) >= 0) {
        opposite_prob <-
          sum(EffSize$Probability[EffSize$Direction == "Negative"])
        if (length(posterior[posterior > 0]) > 0) {
          opposite_max <- min(posterior[posterior > 0])
        } else {
          opposite_max <- 0
        }
        verylarge <- verylarge_pos
        large <- large_pos
        medium <- medium_pos
        small <- small_pos
        verysmall <- verysmall_pos
      } else {
        opposite_prob <-
          sum(EffSize$Probability[EffSize$Direction == "Positive"])
        if (length(posterior[posterior > 0]) > 0) {
          opposite_max <- max(posterior[posterior > 0])
        } else {
          opposite_max <- 0
        }
        verylarge <- verylarge_neg
        large <- large_neg
        medium <- medium_neg
        small <- small_neg
        verysmall <- verysmall_neg
      }

      EffSize_text <- paste0(
        "   - There is a probability of ",
        format_digit(verylarge * 100),
        "% that this effect size is very large, ",
        format_digit(large * 100),
        "% that this effect size is large, ",
        format_digit(medium * 100),
        "% that this effect size is medium, ",
        format_digit(small * 100),
        "% that this effect size is small, ",
        format_digit(verysmall * 100),
        "% that this effect is very small and ",
        format_digit(opposite_prob * 100),
        "% that it has an opposite direction",
        " (between 0 and ", signif(opposite_max, 2), ")."
      )

      values[[varname]]$EffSize <- EffSize
      values[[varname]]$EffSize_text <- EffSize_text

      values[[varname]]$EffSize_VL <- verylarge
      values[[varname]]$EffSize_L <- large
      values[[varname]]$EffSize_M <- medium
      values[[varname]]$EffSize_S <- small
      values[[varname]]$EffSize_VS <- verysmall
      values[[varname]]$EffSize_O <- opposite_prob

      if (varname == "R2") {
        values[[varname]]$EffSize <- NA
        values[[varname]]$EffSize_text <- NA

        values[[varname]]$EffSize_VL <- NA
        values[[varname]]$EffSize_L <- NA
        values[[varname]]$EffSize_M <- NA
        values[[varname]]$EffSize_S <- NA
        values[[varname]]$EffSize_VS <- NA
        values[[varname]]$EffSize_O <- NA
      }
    }
  }


  # Summary
  # -------------
  MPEs <- c()
  for (varname in names(values)) {
    MPEs <- c(MPEs, values[[varname]]$MPE)
  }
  medians <- c()
  for (varname in names(values)) {
    medians <- c(medians, values[[varname]]$median)
  }
  mads <- c()
  for (varname in names(values)) {
    mads <- c(mads, values[[varname]]$mad)
  }
  means <- c()
  for (varname in names(values)) {
    means <- c(means, values[[varname]]$mean)
  }
  sds <- c()
  for (varname in names(values)) {
    sds <- c(sds, values[[varname]]$sd)
  }
  CIs <- c()
  for (varname in names(values)) {
    CIs <- c(CIs, values[[varname]]$CI_values)
  }

  summary <- data.frame(
    Variable = names(values),
    MPE = MPEs,
    Median = medians,
    MAD = mads,
    Mean = means,
    SD = sds,
    CI_lower = CIs[seq(1, length(CIs), 2)],
    CI_higher = CIs[seq(2, length(CIs), 2)]
  )
  names(summary) <- c("Variable", "MPE", "Median", "MAD", "Mean", "SD", paste0(CI, "_CI_lower"), paste0(CI, "_CI_higher"))

  if (effsize == T) {
    EffSizes <- data.frame()
    for (varname in names(values)) {
      Current <- data.frame(
        Very_Large = values[[varname]]$EffSize_VL,
        Large = values[[varname]]$EffSize_L,
        Medium = values[[varname]]$EffSize_M,
        Small = values[[varname]]$EffSize_S,
        Very_Small = values[[varname]]$EffSize_VS,
        Opposite = values[[varname]]$EffSize_O
      )
      EffSizes <- rbind(EffSizes, Current)
    }
    summary <- cbind(summary, EffSizes)
  }



  # Text
  # -------------
  if (effsize) {
    info_effsize <- " Effect sizes are based on Cohen (1988) recommandations."
  } else {
    info_effsize <- ""
  }

  # Model
  info <- paste0(
    "We fitted a Markov Chain Monte Carlo [TYPE] model to predict ",
    outcome,
    " (formula = ", format(fit$formula),
    ").",
    info_effsize,
    " Priors were set as follows: "
  )

  # Priors
  info_priors <- prior_summary(fit)

  if ("adjusted_scale" %in% names(info_priors$prior)) {
    scale <- paste0(
      "), scale = (",
      paste(sapply(info_priors$prior$adjusted_scale, format_digit), collapse = ", ")
    )
  } else {
    scale <- paste0(
      "), scale = (",
      paste(sapply(info_priors$prior$scale, format_digit),collapse = ", ")
    )
  }

  info_priors <- paste0(
    "  ~ ",
    info_priors$prior$dist,
    " (location = (",
    paste(info_priors$prior$location, collapse = ", "),
    scale,
    "))"
  )

  # Coefs
  coefs_text <- c()
  for (varname in names(values)) {
    coefs_text <- c(coefs_text, values[[varname]]$text)
    if (effsize == T) {
      if (!varname %in% c("(Intercept)", "R2")) {
        coefs_text <- c(coefs_text, values[[varname]]$EffSize_text, "")
      }
    }
  }
  text <- c(info, "", info_priors, "", "", paste0(tail(coefs_text, 1), head(coefs_text, 1)), "", head(tail(coefs_text, -1), -1))



  # Plot
  # -------------
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
