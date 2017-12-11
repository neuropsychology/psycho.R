#' Analyze merModLmerTest objects.
#'
#' Analyze merModLmerTest objects.
#'
#' @param x merModLmerTest object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' require(lmerTest)
#' fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#' results <- analyze(fit)
#' summary(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom MuMIn std.coef
#' @import lmerTest
#' @import dplyr
#' @export
analyze.merModLmerTest <- function(x, ...) {


  # Processing
  # -------------
  fit <- x

  R2m <- MuMIn::r.squaredGLMM(fit)["R2m"]
  R2c <- MuMIn::r.squaredGLMM(fit)["R2c"]


  # Summary
  # -------------
  fitsum <- data.frame(lmerTest::summary(fit)$coefficients)

  fitsum$Variable <- rownames(fitsum)
  fitsum$Coef <- fitsum$Estimate
  fitsum$SE <- fitsum$`Std..Error`
  fitsum$df <- as.numeric(fitsum$df)
  fitsum$t <- fitsum$`t.value`
  fitsum$p <- fitsum$`Pr...t..`

  # standardized coefficients
  stdz <- as.data.frame(MuMIn::std.coef(fit, F))
  fitsum$Coef.std <- stdz$Estimate
  fitsum$SE.std <- stdz$`Std. Error`
  fitsum$Effect_Size <- interpret_d(fitsum$Coef.std)

  fitsum <- select_(
    fitsum, "Coef", "SE", "t", "df", "Coef.std", "SE.std",
    "p", "Effect_Size"
  )


  # Varnames
  varnames <- rownames(fitsum)


  # Values
  # -------------
  # Initialize empty values
  values <- list()
  values$R2m <- R2m
  values$R2c <- R2c

  # Loop over all variables
  for (varname in varnames) {
    text <- paste(
      "The effect of ", varname, " was [NOT] significant (beta = ",
      format_digit(fitsum[varname, "Coef"], 2), ", SE = ",
      format_digit(fitsum[varname, "SE"], 2), ", t(",
      format_digit(fitsum[varname, "df"], 2), ") = ",
      format_digit(fitsum[varname, "t"], 2), ", p ",
      format_p(fitsum[varname, "p"]),
      ") and can be considered as ",
      tolower(fitsum[varname, "Effect_Size"]),
      " (std. beta = ",
      format_digit(fitsum[varname, "Coef.std"], 2),
      ", std. SE = ",
      format_digit(fitsum[varname, "SE.std"], 2), ").", sep = ""
    )

    values[[varname]] <- list(
      Coef = fitsum[varname, "Coef"],
      SE = fitsum[varname, "SE"],
      t = fitsum[varname, "t"],
      df = fitsum[varname, "df"],
      Coef.std = fitsum[varname, "Coef.std"],
      SE.std = fitsum[varname, "SE.std"],
      p = fitsum[varname, "p"],
      Effect_Size = fitsum[varname, "Effect_Size"],
      Text = text
    )
  }



  # Text
  # -------------
  text <- c(paste(
    "The overall model predicting ... successfully converged",
    " and explained ", format_digit(R2c * 100, 2),
    "% of the variance of the endogen (the conditional R2). ",
    "The variance explained by the fixed effects was of ",
    format_digit(R2m * 100, 2),
    "% (the marginal R2) and the one explained by the random",
    " effects of ",
    format_digit((R2c - R2m) * 100, 2), "%.", sep = ""
  ))

  for (varname in varnames) {
    text <- c(text, values[[varname]]$Text)
  }



  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = fitsum, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
