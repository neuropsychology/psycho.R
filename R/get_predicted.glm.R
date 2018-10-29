#' Compute predicted values of lm models.
#'
#' Compute predicted from a lm model.
#'
#' @param fit An lm model.
#' @param newdata A data frame in which to look for variables with which to predict. If omitted, the model matrix is used. If "model", the model's data is used.
#' @param prob Probability of confidence intervals (0.9 (default) will compute 2.5-97.5\% CI). Can also be a list of probs (e.g., c(0.90, 0.95)).
#' @param odds_to_probs Transform log odds ratios in logistic models to probabilies.
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
#' fit <- glm(Sex ~ Adjusting, data=affective, family="binomial")
#'
#' refgrid <- psycho::refdata(affective, "Adjusting")
#' predicted <- get_predicted(fit, newdata=refgrid)
#'
#' ggplot(predicted, aes(x=Adjusting, y=Sex_Predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin=Sex_CI_2.5,
#'                   ymax=Sex_CI_97.5),
#'                   alpha=0.1)
#'
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @export
get_predicted.glm <- function(fit, newdata = "model", prob = 0.95, odds_to_probs = TRUE, ...) {


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
        newdata <- fit$data[predictors]
        newdata <- na.omit(fit$data[predictors])
      }
    }
  }


  # Compute ----------------------------------------------------------

  # Predicted Y
  prediction <- as.data.frame(predict(fit, newdata = newdata, type = "link", se.fit = TRUE))
  SE <- as.data.frame(prediction$se.fit)
  pred_y <- as.data.frame(prediction$fit)
  names(pred_y) <- paste0(outcome, "_Predicted")

  # Credible Interval
  for (CI in c(prob)) {
    pred_y_interval <- data.frame(
      lwr = prediction$fit - (qnorm(CI) * SE),
      upr = prediction$fit + (qnorm(CI) * SE)
    )
    names(pred_y_interval) <- paste(outcome, "CI", c((1 - CI) / 2 * 100, 100 - ((1 - CI) / 2 * 100)), sep = "_")
    pred_y <- cbind(pred_y, pred_y_interval)
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
        dplyr::right_join(fit$data[!names(fit$data) %in% predictors] %>%
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
