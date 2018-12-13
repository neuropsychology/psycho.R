#' Compute predicted values of stanreg models.
#'
#' Compute predicted from a stanreg model.
#'
#' @param fit A stanreg model.
#' @param newdata A data frame in which to look for variables with which to predict. If omitted, the model matrix is used. If "model", the model's data is used.
#' @param prob Probability of credible intervals (0.9 (default) will compute 5-95\% CI). Can also be a list of probs (e.g., c(0.90, 0.95)).
#' @param odds_to_probs Transform log odds ratios in logistic models to probabilies.
#' @param keep_iterations Keep all prediction iterations.
#' @param draws An integer indicating the number of draws to return. The default and maximum number of draws is the size of the posterior sample.
#' @param posterior_predict Posterior draws of the outcome instead of the link function (i.e., the regression "line").
#' @param seed An optional seed to use.
#' @param transform If posterior_predict is False, should the linear predictor be transformed using the inverse-link function? The default is FALSE, in which case the untransformed linear predictor is returned.
#' @param re.form If object contains group-level parameters, a formula indicating which group-level parameters to condition on when making predictions. re.form is specified in the same form as for predict.merMod. NULL indicates that all estimated group-level parameters are conditioned on. To refrain from conditioning on any group-level parameters, specify NA or ~0. The newdata argument may include new levels of the grouping factors that were specified when the model was estimated, in which case the resulting posterior predictions marginalize over the relevant variables (see \link[rstanarm]{posterior_predict.stanreg}). If "default", then will ne NULL if the random are present in the data, and NA if not.
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
#' fit <- rstanarm::stan_glm(Tolerating ~ Adjusting, data=affective)
#'
#' refgrid <- psycho::refdata(affective, "Adjusting")
#' predicted <- get_predicted(fit, newdata=refgrid)
#'
#' ggplot(predicted, aes(x=Adjusting, y=Tolerating_Median)) +
#'   geom_line() +
#'   geom_ribbon(aes(ymin=Tolerating_CI_5,
#'                   ymax=Tolerating_CI_95),
#'                   alpha=0.1)
#'
#' fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=affective, family="binomial")
#'
#' refgrid <- psycho::refdata(affective, "Adjusting")
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
#' @import rstanarm
#' @importFrom stats median family model.matrix
#' @importFrom dplyr bind_cols
#' @importFrom tibble rownames_to_column
#' @export
get_predicted.stanreg <- function(fit, newdata = "model", prob = 0.9, odds_to_probs = TRUE, keep_iterations = FALSE, draws = NULL, posterior_predict = FALSE, seed = NULL, transform = FALSE, re.form="default", ...) {

  # Extract names
  predictors <- all.vars(as.formula(fit$formula))
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)

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

  # Deal with potential random
  if(!is.na(re.form)){
    if(re.form=="default"){
      if(is.mixed(fit)){
        # Check if all predictors are in variables
        if(all(get_info(fit)$predictors %in% names(newdata))){
          re.form <- NULL
        } else{
          re.form <- NA
        }
      }
  }
  }

  # Generate draws -------------------------------------------------------
  if (posterior_predict == FALSE) {
    posterior <- rstanarm::posterior_linpred(fit, newdata = newdata, re.form = re.form, seed = seed, draws = draws, transform = transform)
  } else {
    posterior <- rstanarm::posterior_predict(fit, newdata = newdata, re.form = re.form, seed = seed, draws = draws)
  }

  # Format -------------------------------------------------------

  # Predicted Y
  pred_y <- as.data.frame(apply(posterior, 2, median))
  names(pred_y) <- paste0(outcome, "_Median")

  # Credible Interval
  for (CI in c(prob)) {
    pred_y_interval <- HDI(posterior, prob = CI)
    names(pred_y_interval) <- paste(outcome, "CI", c((1 - CI) / 2 * 100, 100 - ((1 - CI) / 2 * 100)), sep = "_")
    pred_y <- cbind(pred_y, pred_y_interval)
  }


  # Keep iterations ---------------------------------------------------------

  if (keep_iterations == TRUE) {
    iterations <- as.data.frame(t(posterior))
    names(iterations) <- paste0("iter_", seq_len(length(names(iterations))))
    pred_y <- cbind(pred_y, iterations)
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
