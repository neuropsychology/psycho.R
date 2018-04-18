#' Compute predicted values of stanreg models.
#'
#' Compute predicted from a stanreg model.
#'
#' @param fit A stanreg model.
#' @param refgrid Do the prediction on the actual dataframe or on a provided refgrid (created with emmeans). See the emmeans::ref_grid function.
#' @param posterior_predict Should the prediction be based on the posterior draws or not.
#' @param prob Probability of credible intervals (0.9 (default) will compute
#' 5-95\% CI).
#' @param keep_iterations Keep all prediction iterations.
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample.
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
#'
#' fit <- rstanarm::stan_glm(Tolerating ~ Adjusting, data=affective, iter=500)
#'
#' refgrid <- emmeans::ref_grid(fit, at=list(
#'     Adjusting=seq(min(affective$Adjusting), max(affective$Adjusting), length.out=2)))
#'
#' predicted <- get_predicted(fit, refgrid)
#'
#'
#' fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=affective, family="binomial", iter=500)
#'
#' refgrid <- emmeans::ref_grid(fit, at=list(
#'     Adjusting=seq(min(affective$Adjusting), max(affective$Adjusting), length.out=5)))
#'
#' predicted <- get_predicted(fit, refgrid, posterior_predict=FALSE)
#'
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_predicted stanreg
#' @import rstanarm
#' @importFrom stats median family
#'
#' @export
get_predicted.stanreg <- function(fit, refgrid=NULL, posterior_predict=FALSE, prob=0.9, keep_iterations=FALSE, draws=NULL, ...) {

  # Info
  data <- fit$data
  formula <- as.character(fit$formula)
  predictors <- all.vars(fit$formula)
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)
  n_predictors <- length(predictors)

  if (is.null(refgrid)) {
    newdata <- NULL
  } else {
    newdata <- refgrid@grid
  }


# Frequentist Predict -------------------------------------------------------
  if (posterior_predict == F) {

    if (is.null(refgrid)) {
      warning("Non-posterior based prediction needs a reference grid. Set posterior_predict to TRUE or include a refgrid.")
      return()
    }
    name_outcome <- paste0("pred_", outcome)

    pred_y <- confint(refgrid, level = prob) %>%
      dplyr::select_("prediction", " asymp.LCL", "asymp.UCL")
    names(pred_y) <- c(
      paste0("pred_", outcome),
      paste0("pred_", outcome, "_", (1 - prob) / 2 * 100),
      paste0("pred_", outcome, "_", 100 - ((1 - prob) / 2 * 100))
    )

    # Add proba if logistic
    if (family(fit)$family == "binomial" & family(fit)$link == "logit") {
      pred_y_prob <- odds_to_probs(pred_y)
      names(pred_y_prob) <- c(
        paste0("pred_", outcome, "_prob"),
        paste0("pred_", outcome, "_", (1 - prob) / 2 * 100, "_prob"),
        paste0("pred_", outcome, "_", 100 - ((1 - prob) / 2 * 100), "_prob")
      )
      pred_y <- cbind(pred_y, pred_y_prob)
    }
    # Posterior Predict -------------------------------------------------------
  } else {

    pred_y_post <- as.matrix(rstanarm::posterior_predict(fit, newdata = newdata))

    pred_y <- c()
    for (var in 1:ncol(pred_y_post)) {
      pred_y <- c(pred_y, median(pred_y_post[, var]))
    }

    pred_y <- as.data.frame(pred_y)
    names(pred_y) <- paste0("pred_", outcome)

    # Confint
    # pred_y_interval <- as.data.frame(rstanarm::posterior_interval(rstanarm::posterior_predict(fit, newdata = newdata), prob = prob))
    pred_y_interval <- hdi(rstanarm::posterior_predict(fit, newdata = newdata), prob=prob)
    names(pred_y_interval) <- paste("pred", outcome, names(pred_y_interval), sep = "_") %>%
      stringr::str_remove("%")


    pred_y <- cbind(pred_y, pred_y_interval)

    # Add proba if logistic
    if (family(fit)$family == "binomial" & family(fit)$link == "logit") {
      pred_y_proba <- c()
      for (var in 1:ncol(pred_y_post)) {
        pred_y_proba <- c(pred_y_proba, mean(pred_y_post[, var]))
      }
      pred_y_proba <- as.data.frame(pred_y_proba)
      names(pred_y_proba) <- paste0("pred_", outcome, "_probability")
      pred_y <- cbind(pred_y, pred_y_proba)
    }

  }


  # Keep iterations ---------------------------------------------------------

  if(keep_iterations == TRUE){
    iterations <- as.data.frame(t(as.matrix(rstanarm::posterior_predict(fit, newdata = newdata, draws=draws))))
    names(iterations) <- paste0("iter_", 1:length(names(iterations)))
    pred_y <- cbind(pred_y, iterations)
  }

# Add predictors ----------------------------------------------------------

  if (is.null(refgrid)) {
    predicted <- cbind(data, pred_y)
  } else {
    predicted <- cbind(newdata, pred_y)
    predicted[".wgt."] <- NULL
  }




  return(predicted)
}


