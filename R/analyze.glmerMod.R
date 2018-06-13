#' Analyze glmerMod objects.
#'
#' Analyze glmerMod objects.
#'
#' @param x merModLmerTest object.
#' @param CI Bootsrapped confidence interval bounds (slow). Set to NULL turn off their computation.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_odds]{interpret_odds}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lme4)
#'
#' fit <- lme4::glmer(vs ~ wt + (1|gear), data=mtcars, family="binomial")
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#'
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom MuMIn std.coef
#' @importFrom stringr str_squish
#' @import lmerTest
#' @import dplyr
#' @export
analyze.glmerMod <- function(x, CI=95, effsize_rules="chen2010", ...) {


  # Processing
  # -------------
  fit <- x

  predictors <- all.vars(as.formula(fit@call$formula))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  R2 <- tryCatch({
    suppressMessages(MuMIn::r.squaredGLMM(fit))
  }, error = function(e) {
    warning("Couldn't compute R2. Might be caused by the presence of missing data.")
    R2 <- c(NA, NA)
    names(R2) <- c("R2m", "R2c")
    return(R2)
  })
  R2m <- R2["R2m"]
  R2c <- R2["R2c"]




  # Summary
  # -------------
  summary <- data.frame(summary(fit)$coefficients)

  summary$Variable <- rownames(summary)
  summary$Coef <- summary$Estimate
  summary$SE <- summary$`Std..Error`
  summary$z <- summary$`z.value`
  summary$p <- summary$`Pr...z..`
  summary$Effect_Size <- c(NA, interpret_odds(tail(summary$Coef, -1), log=TRUE, rules=effsize_rules))


  # Summary
  summary <- dplyr::select_(summary, "Coef", "SE", "z", "p", "Effect_Size")


  if (!is.null(CI)) {
    CI_values <- suppressMessages(confint(fit, level = CI / 100))
    CI_values <- tail(CI_values, n = length(rownames(summary)))
    summary$CI_lower <- CI_values[, 1]
    summary$CI_higher <- CI_values[, 2]
  }


  # Varnames
  varnames <- rownames(summary)


  # Values
  # -------------
  # Initialize empty values
  values <- list(model = list(), effects = list())
  values$model$R2m <- R2m
  values$model$R2c <- R2c

  # Loop over all variables
  for (varname in varnames) {
    if (summary[varname, "p"] < .1) {
      significance <- " "
    } else {
      significance <- " not "
    }

    if (!is.null(CI)) {
      CI_text <- paste0(
        ", ",
        CI, "% CI [",
        format_digit(summary[varname, "CI_lower"], null_treshold = 0.0001),
        ", ",
        format_digit(summary[varname, "CI_higher"], null_treshold = 0.0001),
        "]"
      )
    } else {
      CI_text <- ""
    }



    if (varname == "(Intercept)") {
      text <- paste0(
        "The model's intercept is at ",
        format_digit(summary[varname, "Coef"], 2),
        " (SE = ",
        format_digit(summary[varname, "SE"], 2),
        CI_text,
        "). Within this model:"
      )
    } else{
      text <- paste0(
        "The effect of ",
        varname,
        " is",
        significance,
        "significant (beta = ",
        format_digit(summary[varname, "Coef"], 2),
        ", SE = ",
        format_digit(summary[varname, "SE"], 2),
        CI_text,
        ", z = ",
        format_digit(summary[varname, "z"], 2),
        ", p ",
        format_p(summary[varname, "p"]),
        ") and can be considered as ",
        tolower(summary[varname, "Effect_Size"]),
        "."
      )
    }

    values$effects[[varname]] <- list(
      Coef = summary[varname, "Coef"],
      SE = summary[varname, "SE"],
      z = summary[varname, "z"],
      p = summary[varname, "p"],
      Effect_Size = summary[varname, "Effect_Size"],
      Text = text
    )
  }



  # Text
  # -------------
  text <- c(paste0(
    "The overall model predicting ",
    outcome,
    " (formula = ",
    stringr::str_squish(format(fit@call$formula)),
    ") successfully converged and explained ",
    format_digit(R2c * 100, 2), "% of the variance of the",
    " endogen (the conditional R2). ",
    "The variance explained by the fixed effects was of ",
    format_digit(R2m * 100, 2), "% (the marginal R2) and the ",
    "one explained by the random effects of ",
    format_digit((R2c - R2m) * 100, 2), "%. ",
    values$effects[["(Intercept)"]]$Text
  ))

  for (varname in varnames) {
    if (varname != "(Intercept)") {
      text <- c(text, paste("   -", values$effects[[varname]]$Text))
    }
  }



  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
