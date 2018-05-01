#' Analyze lmerModLmerTest objects.
#'
#' Analyze lmerModLmerTest objects.
#'
#' @param x lmerModLmerTest object.
#' @param CI Bootsrapped confidence interval bounds (slow). Set to NULL turn off their computation.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lmerTest)
#' fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom MuMIn std.coef
#' @import dplyr
#' @export
analyze.lmerModLmerTest <- function(x, CI=95, ...) {


  # Processing
  # -------------
  fit <- x

  predictors <- all.vars(as.formula(eval(fit@call$formula)))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  R2m <- MuMIn::r.squaredGLMM(fit)["R2m"]
  R2c <- MuMIn::r.squaredGLMM(fit)["R2c"]


  # TODO: Bootstrapped p values
  # boot.out = lme4::bootMer(fit, lme4::fixef, nsim=1000) #nsim determines p-value decimal places
  # p = rbind(
  #   (1-apply(boot.out$t<0, 2, mean))*2,
  #   (1-apply(boot.out$t>0, 2, mean))*2)
  # p = apply(p, 2, min)



  # Summary
  # -------------
  fitsum <- data.frame(summary(fit)$coefficients)

  fitsum$Variable <- rownames(fitsum)
  fitsum$Coef <- fitsum$Estimate
  fitsum$SE <- fitsum$`Std..Error`
  fitsum$df <- as.numeric(fitsum$df)
  fitsum$t <- fitsum$`t.value`
  fitsum$p <- fitsum$`Pr...t..`

  # standardized coefficients
  stdz <- as.data.frame(MuMIn::std.coef(fit, partial.sd = FALSE))
  fitsum$Coef.std <- stdz$Estimate
  fitsum$SE.std <- stdz$`Std. Error`
  fitsum$Effect_Size <- interpret_d(fitsum$Coef.std)

  fitsum <- dplyr::select_(
    fitsum, "Variable", "Coef", "SE", "t", "df", "Coef.std", "SE.std",
    "p", "Effect_Size"
  )

  if (!is.null(CI)) {
    CI_values <- suppressMessages(confint(fit, level = CI / 100))
    CI_values <- tail(CI_values, n = length(rownames(fitsum)))
    fitsum$CI_lower <- CI_values[, 1]
    fitsum$CI_higher <- CI_values[, 2]
  }


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
      significance <- ""
    } else {
      significance <- "not"
    }

    if (!is.null(CI)) {
      CI_text <- paste0(
        ", ",
        CI, "% CI [",
        format_digit(fitsum[varname, "CI_lower"], null_treshold = 0.0001),
        ", ",
        format_digit(fitsum[varname, "CI_higher"], null_treshold = 0.0001),
        "]"
      )
    } else {
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
      ", t(",
      format_digit(fitsum[varname, "df"], 2), ") = ",
      format_digit(fitsum[varname, "t"], 2), ", p ",
      format_p(fitsum[varname, "p"]),
      ") and can be considered as ",
      tolower(fitsum[varname, "Effect_Size"]),
      " (std. beta = ",
      format_digit(fitsum[varname, "Coef.std"], 2),
      ", std. SE = ",
      format_digit(fitsum[varname, "SE.std"], 2), ")."
    )

    if (varname == "(Intercept)") {
      text <- paste0(
        "The model's intercept is at ",
        format_digit(fitsum[varname, "Coef"], 2),
        " (SE = ",
        format_digit(fitsum[varname, "SE"], 2),
        CI_text,
        "). Within this model:"
      )
    }

    values$effects[[varname]] <- list(
      Coef = fitsum[varname, "Coef"],
      SE = fitsum[varname, "SE"],
      CI_lower = fitsum[varname, "CI_lower"],
      CI_higher = fitsum[varname, "CI_higher"],
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
  text <- c(paste0(
    "The overall model predicting ",
    outcome,
    " (formula = ",
    paste0(format(eval(fit@call$formula)), collapse = ""),
    ") successfully converged",
    " and explained ",
    format_digit(R2c * 100, 2),
    "% of the variance of the endogen (the conditional R2). ",
    "The variance explained by the fixed effects was of ",
    format_digit(R2m * 100, 2),
    "% (the marginal R2) and the one explained by the random",
    " effects of ",
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

  output <- list(text = text, plot = plot, summary = fitsum, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
