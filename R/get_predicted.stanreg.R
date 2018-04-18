#' Compute predicted values of stanreg models.
#'
#' Compute predicted from a stanreg model.
#'
#' @param fit A stanreg model.
#' @param newdata A data frame in which to look for variables with which to predict. If omitted, the model matrix is used. If "model", the model's data is used.
#' @param prob Probability of credible intervals (0.9 (default) will compute 5-95\% CI). Can also be a list of probs (e.g., c(0.90, 0.95)).
#' @param keep_iterations Keep all prediction iterations.
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample.
#' @param odds_to_probs Transform log odds ratios in logistic models to probabilies.
#' @param posterior_predict Posterior draws of the outcome instead of the link function (i.e., the regression "line").
#' @param seed An optional seed to use.
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
#' require(rstanarm)
#'
#' fit <- rstanarm::stan_glm(Tolerating ~ Adjusting, data=affective, iter=500)
#'
#' refgrid <- emmeans::ref_grid(fit, at=list(
#'     Adjusting=seq(min(affective$Adjusting), max(affective$Adjusting), length.out=10)))
#'
#' predicted <- get_predicted(fit, newdata=refgrid)
#'
#' ggplot(predicted, aes(x=Adjusting, y=Tolerating_Median)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin=Tolerating_CI_5,
#'                   ymax=Tolerating_CI_95),
#'                   alpha=0.1)
#'
#' fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=affective, family="binomial", iter=500)
#'
#' refgrid <- emmeans::ref_grid(fit, at=list(
#'     Adjusting=seq(min(affective$Adjusting), max(affective$Adjusting), length.out=10)))
#'
#' predicted <- get_predicted(fit, newdata=refgrid)
#'
#' ggplot(predicted, aes(x=Adjusting, y=Sex_Median)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin=Sex_CI_5,
#'                   ymax=Sex_CI_95),
#'                   alpha=0.1)
#'
#' }
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method get_predicted stanreg
#' @import rstanarm
#' @importFrom stats median family model.matrix
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @export
get_predicted.stanreg <- function(fit, newdata="model", prob=0.9, keep_iterations=FALSE, draws=NULL, odds_to_probs=TRUE, posterior_predict=FALSE, seed=NULL, ...) {

  # Extract names
  predictors <- all.vars(fit$formula)
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

  # Set newdata if refgrid
  if("emmGrid" %in% class(newdata)) {
    newdata <- newdata@grid
    newdata[".wgt."] <- NULL
  }

  # Deal with potential random
  re.form = NULL
  if(!is.null(newdata)){
    if(newdata != "model" & is.mixed(fit)){
      re.form = NA
    }
  }

  # Set newdata to actual data
  original_data <- FALSE
  if(!is.null(newdata)){
    if(newdata == "model"){
      original_data <- TRUE
      newdata <- fit$data[predictors]
      newdata <- na.omit(fit$data[predictors])
    }
  }

# Generate draws -------------------------------------------------------
  if (posterior_predict == F) {
    posterior <- rstanarm::posterior_linpred(fit, newdata = newdata, re.form=re.form, seed=seed)
  } else{
    posterior <- rstanarm::posterior_predict(fit, newdata = newdata, re.form=re.form, seed=seed)
  }

# Format -------------------------------------------------------

  # Predicted Y
  pred_y <- as.data.frame(apply(posterior, 2, median))
  names(pred_y) <- paste0(outcome, "_Median")

  # Credible Interval
  for(CI in c(prob)){
    pred_y_interval <- hdi(posterior, prob=CI)
    names(pred_y_interval) <- paste(outcome, "CI", c((1 - CI) / 2 * 100, 100 - ((1 - CI) / 2 * 100)), sep="_")
    pred_y <- cbind(pred_y, pred_y_interval)
  }


# Keep iterations ---------------------------------------------------------

  if(keep_iterations == TRUE){
    iterations <- as.data.frame(t(posterior))
    names(iterations) <- paste0("iter_", 1:length(names(iterations)))
    pred_y <- cbind(pred_y, iterations)
  }

# Transform odds to probs ----------------------------------------------------------

  if (family(fit)$family == "binomial" & family(fit)$link == "logit") {
    pred_y <- odds_to_probs(pred_y)
  }


# Add predictors ----------------------------------------------------------


  if(!is.null(newdata)){
    if(original_data){
      predicted <- newdata %>%
        tibble::rownames_to_column() %>%
        dplyr::bind_cols(pred_y) %>%
        dplyr::right_join(fit$data[!names(fit$data) %in% predictors] %>%
                           tibble::rownames_to_column(),
                         by="rowname") %>%
        select_("-rowname")
    } else{
      predicted <- dplyr::bind_cols(newdata, pred_y)
    }
  } else{
    predicted <- dplyr::bind_cols(as.data.frame(model.matrix(fit)), pred_y)
  }


  return(predicted)
}


