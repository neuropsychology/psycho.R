#' Analyze lm objects.
#'
#' Analyze lm objects.
#'
#' @param x lm object.
#' @param CI Confidence interval bounds. Set to NULL turn off their computation.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' fit <- lm(Sepal.Length ~ Sepal.Width, data=iris)
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import dplyr
#' @importFrom stats formula
#' @export
analyze.lm <- function(x, CI=95, ...) {


  # Processing
  # -------------
  fit <- x

  predictors <- all.vars(stats::formula(fit))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  R2 <- summary(fit)$r.squared
  R2adj <- summary(fit)$adj.r.squared

  # Summary
  # -------------
  fitsum <- data.frame(summary(fit)$coefficients)

  fitsum$Variable <- rownames(fitsum)
  fitsum$Coef <- fitsum$Estimate
  fitsum$SE <- fitsum$`Std..Error`
  fitsum$t <- fitsum$`t.value`
  fitsum$p <- fitsum$`Pr...t..`

  # standardized coefficients
  stdz <- as.data.frame(MuMIn::std.coef(fit, partial.sd=FALSE))
  fitsum$Coef.std <- stdz$Estimate
  fitsum$SE.std <- stdz$`Std. Error`
  fitsum$Effect_Size <- interpret_d(fitsum$Coef.std)

  fitsum <- dplyr::select_(
    fitsum, "Variable", "Coef", "SE", "t","Coef.std", "SE.std",
    "p", "Effect_Size"
  )

  if(!is.null(CI)){
    CI_values <- confint(fit, level=CI/100)
    CI_values <- tail(CI_values, n=2)
    fitsum$CI_lower <- CI_values[,1]
    fitsum$CI_higher <- CI_values[,2]
  }


  # Varnames
  varnames <- rownames(fitsum)



  # Values
  # -------------
  # Initialize empty values
  values <- list(model = list(), effects = list())
  values$model$R2 <- R2
  values$model$R2adj <- R2adj


  # Loop over all variables
  for (varname in varnames) {
    if (fitsum[varname, "p"] < .1) {
      significance <- ""
    } else {
      significance <- "not"
    }

    if(!is.null(CI)){
      CI_text <- paste0(", ",
                        CI, "% CI [",
                        format_digit(fitsum[varname, "CI_lower"], null_treshold = 0.0001),
                        ", ",
                        format_digit(fitsum[varname, "CI_higher"], null_treshold = 0.0001),
                        "])")
    } else{
      CI_text <- ""
    }



    text <- paste0(
      "The effect of ",
      varname,
      " is ",
      significance,
      " significant (beta = ",
      format_digit(fitsum[varname, "Coef"], 2), ", SE = ",
      format_digit(fitsum[varname, "SE"], 2),
      CI_text,
      ", t = ",
      format_digit(fitsum[varname, "t"], 2), ", p ",
      format_p(fitsum[varname, "p"]),
      ") and can be considered as ",
      tolower(fitsum[varname, "Effect_Size"]),
      " (std. beta = ",
      format_digit(fitsum[varname, "Coef.std"], 2),
      ", std. SE = ",
      format_digit(fitsum[varname, "SE.std"], 2), ")."
    )

    values$effects[[varname]] <- list(
      Coef = fitsum[varname, "Coef"],
      SE = fitsum[varname, "SE"],
      CI_lower = fitsum[varname, "CI_lower"],
      CI_higher = fitsum[varname, "CI_higher"],
      t = fitsum[varname, "t"],
      Coef.std = fitsum[varname, "Coef.std"],
      SE.std = fitsum[varname, "SE.std"],
      p = fitsum[varname, "p"],
      Effect_Size = fitsum[varname, "Effect_Size"],
      Text = text
    )
  }



  # Text
  # -------------
  text <- c(paste0(
    "The overall model predicting ",
    outcome,
    " (formula = ",
    paste0(format(stats::formula(fit)), collapse=""),
    ") successfully converged",
    " and explained ",
    format_digit(R2 * 100, 2),
    "% of the variance of the endogen (adjusted R2 = ",
    format_digit(R2adj * 100, 2),
    "). Within this model:"
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
