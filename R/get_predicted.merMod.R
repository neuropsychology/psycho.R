#' Compute predicted values of lm models.
#'
#' Compute predicted from a lm model.
#'
#' @param fit An lm model.
#' @param newdata A data frame in which to look for variables with which to predict. If omitted, the model matrix is used. If "model", the model's data is used.
#' @param prob Probability of confidence intervals (0.95 will compute 2.5-97.5\% CI). Can also be a list of probs (e.g., c(0.90, 0.95)). Default to NULL as it takes a very long time to compute (see \link[lme4]{bootMer}).
#' @param odds_to_probs Transform log odds ratios in logistic models to probabilies.
#' @param iter An integer indicating the number of iterations for bootstrapping (when prob is not null).
#' @param seed An optional seed to use.
#' @param re.form Formula for random effects to condition on. If NULL, include all random effects; if NA or ~0, include no random effects (see \link[lme4]{predict.merMod}). If "default", then will ne NULL if the random are present in the data, and NA if not.
#' @param use.u logical, indicating whether the spherical random effects should be simulated / bootstrapped as well. If TRUE, they are not changed, and all inference is conditional on these values. If FALSE, new normal deviates are drawn (see\link[lme4]{bootMer}).
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return dataframe with predicted values.
#'
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(ggplot2)
#' 
#' fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1 | Salary), data = affective)
#' 
#' refgrid <- psycho::refdata(affective, "Adjusting")
#' predicted <- get_predicted(fit, newdata = refgrid)
#' 
#' ggplot(predicted, aes(x = Adjusting, y = Tolerating_Predicted)) +
#'   geom_line()
#' 
#' predicted <- get_predicted(fit, newdata = refgrid, prob = 0.95, iter = 100) # Takes a long time
#' 
#' ggplot(predicted, aes(x = Adjusting, y = Tolerating_Predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(
#'     ymin = Tolerating_CI_2.5,
#'     ymax = Tolerating_CI_97.5
#'   ),
#'   alpha = 0.1
#'   )
#' 
#' 
#' 
#' fit <- lme4::glmer(Sex ~ Adjusting + (1 | Salary), data = affective, family = "binomial")
#' 
#' refgrid <- psycho::refdata(affective, "Adjusting")
#' predicted <- get_predicted(fit, newdata = refgrid)
#' 
#' ggplot(predicted, aes(x = Adjusting, y = Sex_Predicted)) +
#'   geom_line()
#' 
#' predicted <- get_predicted(fit, newdata = refgrid, prob = 0.95, iter = 100) # Takes a long time
#' 
#' ggplot(predicted, aes(x = Adjusting, y = Sex_Predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(
#'     ymin = Sex_CI_2.5,
#'     ymax = Sex_CI_97.5
#'   ),
#'   alpha = 0.1
#'   )
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @export
get_predicted.merMod <- function(fit, newdata = "model", prob = NULL, odds_to_probs = TRUE, iter = 100, seed = NULL, re.form = "default", use.u = FALSE, ...) {


  # Extract names
  info <- get_info(fit)
  outcome <- info$outcome
  predictors <- info$predictors

  # Set newdata if refgrid
  if ("emmGrid" %in% class(newdata)) {
    newdata <- newdata@grid
    newdata[".wgt."] <- NULL
  }

  # Set newdata to actual data
  original_data <- FALSE
  if (!is.null(newdata)) {
    if (is.character(newdata)) {
      if (newdata == "model") {
        original_data <- TRUE
        newdata <- na.omit(fit@frame)
      }
    }
  }


  # Deal with random
  if (!is.na(re.form)) {
    if (re.form == "default") {
      # Check if all predictors are in variables
      if (all(get_info(fit)$predictors %in% names(newdata))) {
        re.form <- NULL
      } else {
        re.form <- NA
      }
    }
  }



  # Compute ----------------------------------------------------------

  pred_y <- as.data.frame(predict(fit, newdata = newdata, re.form = re.form))
  names(pred_y) <- paste0(outcome, "_Predicted")

  if (!is.null(prob)) {
    predFun <- function(fit) {
      predict(fit, newdata, newdata = newdata, re.form = re.form)
    }
    predMat <- lme4::bootMer(fit, nsim = iter, FUN = predFun, use.u = use.u, seed = seed)$t

    for (CI in c(prob)) {
      pred_y_interval <- as.data.frame(t(apply(predMat, 2, quantile, c((1 - CI) / 2, CI + (1 - CI) / 2), na.rm = TRUE)))
      names(pred_y_interval) <- paste(outcome, "CI", c((1 - CI) / 2 * 100, 100 - ((1 - CI) / 2 * 100)), sep = "_")
      pred_y <- cbind(pred_y, pred_y_interval)
    }
  }


  # Transform odds to probs ----------------------------------------------------------

  if (family(fit)$family == "binomial" & family(fit)$link == "logit") {
    if (odds_to_probs == TRUE) {
      pred_y <- odds_to_probs(pred_y)
    }
  }


  # Add predictors ----------------------------------------------------------


  if (!is.null(newdata)) {
    if (original_data) {
      predicted <- newdata %>%
        tibble::rownames_to_column() %>%
        dplyr::bind_cols(pred_y) %>%
        dplyr::right_join(fit@frame[!names(fit@frame) %in% names(newdata)] %>%
          tibble::rownames_to_column(),
        by = "rowname"
        ) %>%
        select_("-rowname")
    } else {
      predicted <- dplyr::bind_cols(newdata, pred_y)
    }
  } else {
    predicted <- dplyr::bind_cols(as.data.frame(model.matrix(fit)), pred_y)
  }


  return(predicted)
}
