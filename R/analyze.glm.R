#' Analyze glm objects.
#'
#' Analyze glm objects.
#'
#' @param x glm object.
#' @param CI Confidence interval bounds. Set to NULL turn off their computation.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_odds]{interpret_odds}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' fit <- glm(Sex ~ Adjusting, data=psycho::affective, family="binomial")
#'
#' results <- analyze(fit)
#' summary(results)
#' print(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @references Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' @import dplyr
#' @importFrom stats formula
#' @importFrom stringr str_squish
#' @export
analyze.glm <- function(x, CI=95, effsize_rules="cohen1988", ...) {


  # Processing
  # -------------
  fit <- x

  if (fit$family$family != "binomial") {
    stop(paste("Models of family", fit$family$family, "not supported yet."))
  }

  info <- get_info(fit)
  outcome <- info$outcome
  predictors <- info$predictors

  # R2 <- tjur_D(fit)
  R2 <- get_R2(fit)

  # Summary
  # -------------
  summary <- data.frame(summary(fit)$coefficients)

  summary$Variable <- rownames(summary)
  summary$Coef <- summary$Estimate
  summary$SE <- summary$`Std..Error`
  summary$z <- summary$`z.value`
  summary$p <- summary$`Pr...z..`

  # standardized coefficients
  summary <- cbind(summary, standardize(fit, method = "agresti"))
  summary$Effect_Size <- interpret_odds(summary$Coef.std, log = TRUE, rules = effsize_rules)

  summary <- dplyr::select_(
    summary, "Variable", "Coef", "SE", "z", "Coef.std", "SE.std",
    "p", "Effect_Size"
  )

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



    text <- paste0(
      "The effect of ",
      varname,
      " is",
      significance,
      "significant (beta = ",
      format_digit(summary[varname, "Coef"], 2), ", SE = ",
      format_digit(summary[varname, "SE"], 2),
      CI_text,
      ", z = ",
      format_digit(summary[varname, "z"], 2), ", p ",
      format_p(summary[varname, "p"]),
      ") and can be considered as ",
      tolower(summary[varname, "Effect_Size"]),
      " (std. beta = ",
      format_digit(summary[varname, "Coef.std"], 2),
      ", std. SE = ",
      format_digit(summary[varname, "SE.std"], 2), ")."
    )

    if (varname == "(Intercept)") {
      text <- paste0(
        "The model's intercept is at ",
        format_digit(summary[varname, "Coef"], 2),
        " (SE = ",
        format_digit(summary[varname, "SE"], 2),
        CI_text,
        "). Within this model:"
      )
    }

    values$effects[[varname]] <- list(
      Coef = summary[varname, "Coef"],
      SE = summary[varname, "SE"],
      CI_lower = summary[varname, "CI_lower"],
      CI_higher = summary[varname, "CI_higher"],
      z = summary[varname, "z"],
      Coef.std = summary[varname, "Coef.std"],
      SE.std = summary[varname, "SE.std"],
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
    stringr::str_squish(paste0(format(stats::formula(fit)), collapse = "")),
    ") has an explanatory power of ",
    format_digit(R2 * 100, 2),
    "%. ",
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
