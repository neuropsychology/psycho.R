#' Compute predicted values of stanreg models.
#'
#' Compute predicted from a stanreg model.
#'
#' @param fit A stanreg model.
#' @param refgrid Do the prediction on the actual dataframe or on a provided refgrid (created with emmeans). See the emmeans::ref_grid function.
#' @param posterior_predict Should the prediction be based on the posterior draws or not.
#' @param prob Probability of credible intervals (0.9 (default) will compute
#' 5-95\% CI).
#' @param draws Precision of the estimate.
#' @param precision Precision of the new dataframe to be generated.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @return dataframe with predicted values.
#'
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' require(rstanarm)
#' fit <- rstanarm::stan_glm(vs ~ mpg * cyl, data=mtcars)
#'
#' predicted <- get_predicted(fit)
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_predicted stanreg
#' @import rstanarm
#' @importFrom stats median
#'
#' @export
get_predicted.stanreg <- function(fit, refgrid=NULL, posterior_predict=F, prob=0.9, draws=1000, precision=10, ...) {
  data <- fit$data
  formula <- as.character(fit$formula)
  predictors <- all.vars(fit$formula)
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)
  n_predictors <- length(predictors)
  length_out <- precision

  if (is.null(refgrid)) {
    newdata <- NULL
  } else {
    newdata <- refgrid@grid
  }

  if (posterior_predict == F) {
    if (is.null(refgrid)) {
      warning("Non-posterior based prediction needs a reference grid. Set posterior_predict to TRUE or include a refgrid")
      return()
    }
    name_outcome <- paste0("pred_", outcome)

    pred_y <- confint(refgrid, level = prob) %>%
      dplyr::select_("prediction", " asymp.LCL", "asymp.UCL")
    names(pred_y) <- c(
      paste0("pred_", outcome),
      paste0("pred_", outcome, "_", (1 - prob) / 2 * 100, "%"),
      paste0("pred_", outcome, "_", 100 - ((1 - prob) / 2 * 100), "%")
    )
  } else {
    pred_y_post <- as.matrix(rstanarm::posterior_predict(fit, newdata = newdata, draws = draws))
    # posterior_linpred
    pred_y <- c()


    for (var in 1:ncol(pred_y_post)) {
      pred_y <- c(pred_y, median(pred_y_post[, var]))
    }

    pred_y <- as.data.frame(pred_y)
    names(pred_y) <- paste0("pred_", outcome)

    pred_y_interval <- as.data.frame(rstanarm::posterior_interval(rstanarm::posterior_predict(fit, newdata = newdata, draws = draws), prob = prob, draws = draws))
    names(pred_y_interval) <- paste("pred", outcome, names(pred_y_interval), sep = "_")

    pred_y <- cbind(pred_y, pred_y_interval)

    # If Binary, add proba
    if (length(unique(data[[outcome]])) == 2 & 0 %in% unique(data[[outcome]]) & 1 %in% unique(data[[outcome]])) {
      pred_y_proba <- c()
      for (var in 1:ncol(pred_y_post)) {
        pred_y_proba <- c(pred_y_proba, mean(pred_y_post[, var]))
      }
      pred_y_proba <- as.data.frame(pred_y_proba)
      names(pred_y_proba) <- paste0("pred_", outcome, "_probability")
      pred_y <- cbind(pred_y, pred_y_proba)
    }
  }



  if (is.null(refgrid)) {
    predicted <- cbind(data, pred_y)
  } else {
    predicted <- cbind(newdata, pred_y)
    predicted[".wgt."] <- NULL
  }


  return(predicted)
}
