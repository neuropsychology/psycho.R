#' Compute predicted values of lm models.
#'
#' Compute predicted from a lm model.
#'
#' @param fit An lm model.
#' @param newdata A data frame in which to look for variables with which to predict. If omitted, the model matrix is used. If "model", the model's data is used.
#' @param prob Probability of credible intervals (0.9 (default) will compute 5-95\% CI). Can also be a list of probs (e.g., c(0.90, 0.95)).
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
#' fit <- lm(Tolerating ~ Adjusting, data=affective)
#'
#' refgrid <- psycho::refdata(affective, "Adjusting")
#' predicted <- get_predicted(fit, newdata=refgrid)
#'
#' ggplot(predicted, aes(x=Adjusting, y=Tolerating_Predicted)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin=Tolerating_CI_2.5,
#'                   ymax=Tolerating_CI_97.5),
#'                   alpha=0.1)
#'
#'
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import rstanarm
#' @importFrom stats median family model.matrix
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @export
get_predicted.lm <- function(fit, newdata="model", prob=0.95, ...) {


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
        newdata <- as.data.frame(fit$model[predictors])
        newdata <- na.omit(fit$model[predictors])
      }
    }
  }


  # Compute ----------------------------------------------------------

  # Predicted Y
  pred_y <- as.data.frame(predict(fit, newdata))
  names(pred_y) <- paste0(outcome, "_Predicted")

  # Credible Interval
  for (CI in c(prob)) {
    pred_y_interval <- as.data.frame(predict(fit, newdata, interval = "confidence", level = CI)[, -1])
    names(pred_y_interval) <- paste(outcome, "CI", c((1 - CI) / 2 * 100, 100 - ((1 - CI) / 2 * 100)), sep = "_")
    pred_y <- cbind(pred_y, pred_y_interval)
  }



  # Add predictors ----------------------------------------------------------
  if (!is.null(newdata)) {
    if (original_data) {
      predicted <- newdata %>%
        tibble::rownames_to_column() %>%
        dplyr::bind_cols(pred_y) %>%
        dplyr::right_join(fit$model[!names(fit$model) %in% predictors] %>%
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
