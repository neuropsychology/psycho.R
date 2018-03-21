#' Compute predicted values of stanreg models.
#'
#' Compute predicted from a stanreg model.
#'
#' @param fit A stanreg model.
#' @param prob Probability of credible intervals (0.9 (default) will compute
#' 5-95\% CI).
#' @param draws Precision of the estimate.
#' @param newdf Should the predictions be based on actual data or,
#' generate a new dataframe based on all combinations of values
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
get_predicted.stanreg <- function(fit, prob=0.9, draws=500, newdf=FALSE, precision=10, ...){

  data <- fit$data
  formula <- as.character(fit$formula)
  predictors <- all.vars(fit$formula)
  outcome <- predictors[[1]]
  predictors <- tail(predictors, -1)
  n_predictors <- length(predictors)
  length_out <- precision


  if(newdf==T){

    info <- list()
    for(var in predictors){
      info[[var]] <- list()

      if(is.factor(data[[var]])){
        info[[var]]$type <- "factor"
        info[[var]]$uniques <- unique(data[[var]])
        info[[var]]$data <- rep(info[[var]]$uniques, length.out=length_out)
      } else{
        info[[var]]$type <- "num"
        info[[var]]$min <- min(data[[var]])
        info[[var]]$max <- max(data[[var]])
        info[[var]]$data <- seq(info[[var]]$min, info[[var]]$max, length.out=length_out)
        info[[var]]$uniques <- unique(info[[var]]$data)
      }
    }

    predicted <- data.frame()
    for(var in predictors){
      other_predictors <- predictors[predictors != var]

      for(value in info[[var]]$uniques){
        temp <- data.frame("x" = rep(NA, length.out=length_out))
        temp[var] <- rep(value, length.out=length_out)

        for(other_pred in other_predictors){
          temp[other_pred] <- info[[other_pred]]$data
        }

        predicted <- rbind(predicted, temp)
      }
    }
    predicted <- dplyr::select_(predicted, "-x")
  } else{
    predicted <- NULL
  }



  pred_y_post <- as.data.frame(rstanarm::posterior_predict(fit, newdata = predicted, draws = 500))
  pred_y <- c()


  for(i in pred_y_post){
    pred_y <- c(pred_y, median(i))
  }
  pred_y = as.data.frame(pred_y)
  names(pred_y) <- paste0("pred_", outcome, "_median")

  pred_y_interval <- as.data.frame(rstanarm::posterior_interval(rstanarm::posterior_predict(fit, newdata = predicted, draws = draws), prob=prob, draws = draws))
  names(pred_y_interval) <- paste("pred", outcome, names(pred_y_interval), sep="_")

  pred_y <- cbind(pred_y_interval, pred_y)

  # If Binary, add proba
  if(length(unique(data[[outcome]])) == 2 & 0 %in% unique(data[[outcome]]) & 1 %in% unique(data[[outcome]])){
    pred_y_proba <- c()
    for(i in pred_y_post){
      pred_y_proba <- c(pred_y_proba, mean(i))
        }
      pred_y_proba = as.data.frame(pred_y_proba)
      names(pred_y_proba) <- paste0("pred_", outcome, "_probability")
    pred_y <- cbind(pred_y, pred_y_proba)
  }



  if(newdf==T){
    predicted <- cbind(predicted, pred_y)
  } else{
    predicted <- data
    predicted <- cbind(predicted, pred_y)
  }

  return(predicted)
}
