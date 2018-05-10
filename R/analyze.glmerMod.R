#' Analyze glmerMod objects.
#'
#' Analyze glmerMod objects.
#'
#' @param x merModLmerTest object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lme4)
#' fit <- lme4::glmer(vs ~ mpg + (1|cyl), data=mtcars, family="binomial")
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom MuMIn std.coef
#' @importFrom stringr str_squish
#' @import lmerTest
#' @import dplyr
#' @export
analyze.glmerMod <- function(x, ...) {


  # Processing
  # -------------
  fit <- x

  predictors <- all.vars(as.formula(fit@call$formula))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  R2m <- MuMIn::r.squaredGLMM(fit)["R2m"]
  R2c <- MuMIn::r.squaredGLMM(fit)["R2c"]


  # Summary
  # -------------
  fitsum <- data.frame(summary(fit)$coefficients)

  fitsum$Variable <- rownames(fitsum)
  fitsum$Coef <- fitsum$Estimate
  fitsum$SE <- fitsum$`Std..Error`
  fitsum$z <- fitsum$`z.value`
  fitsum$p <- fitsum$`Pr...z..`

  # standardized coefficients
  fitsum <- select_(fitsum, "Coef", "SE", "z", "p")


  # Varnames
  varnames <- rownames(fitsum)


  # Values
  # -------------
  # Initialize empty values
  values <- list(model = list(), effects = list())
  values$model$R2m <- R2m
  values$model$R2c <- R2c

  # Loop over all variables
  for (varname in varnames) {
    if (fitsum[varname, "p"] < .1) {
      significance <- " "
    } else {
      significance <- " not "
    }


    text <- paste0(
      "The effect of ",
      varname,
      " is",
      significance,
      "significant (beta = ",
      format_digit(fitsum[varname, "Coef"], 2), ", SE = ",
      format_digit(fitsum[varname, "SE"], 2), ", z = ",
      format_digit(fitsum[varname, "z"], 2), ", p ",
      format_p(fitsum[varname, "p"]), ")."
    )

    values$effects[[varname]] <- list(
      Coef = fitsum[varname, "Coef"],
      SE = fitsum[varname, "SE"],
      z = fitsum[varname, "z"],
      p = fitsum[varname, "p"],
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
    ") successfully converged",
    "and explained ",
    format_digit(R2c * 100, 2), "% of the variance of the",
    " endogen (the conditional R2). ",
    "The variance explained by the fixed effects was of ",
    format_digit(R2m * 100, 2), "% (the marginal R2) and the ",
    "one explained by the random effects of ",
    format_digit((R2c - R2m) * 100, 2), "%."
  ))

  for (varname in varnames) {
    text <- c(text, paste("   -", values$effects[[varname]]$Text))
  }



  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = fitsum, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
