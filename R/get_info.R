#' Get information about objects.
#'
#' Get information about models.
#'
#'
#' @param x object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lme4)
#' 
#' fit <- lme4::glmer(vs ~ wt + (1 | gear), data = mtcars, family = "binomial")
#' 
#' info <- get_info(fit)
#' info
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_info <- function(x, ...) {
  UseMethod("get_info")
}










#' Get information about models.
#'
#' Get information about models.
#'
#' @param x object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lme4)
#' 
#' fit <- lme4::glmer(vs ~ wt + (1 | gear), data = mtcars, family = "binomial")
#' 
#' info <- get_info(fit)
#' info
#' 
#' #
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_info.lmerModLmerTest <- function(x, ...) {
  fit <- x

  info <- tryCatch({

    # Get formula
    formula <- get_formula(fit)
    # Get variables
    predictors <- all.vars(formula)
    outcome <- predictors[[1]]
    predictors <- tail(predictors, -1)
    random <- names(ranef(fit))[names(ranef(fit)) %in% predictors]
    predictors <- predictors[!predictors %in% random]

    return(list(
      formula = formula,
      predictors = predictors,
      outcome = outcome,
      random = random
    ))
  }, error = function(e) {

    # Get formula
    formula <- get_formula(fit)
    # Get variables
    predictors <- NA
    outcome <- "Y"
    random <- NA

    return(list(
      formula = formula,
      predictors = predictors,
      outcome = outcome,
      random = random
    ))
  })

  return(info)
}
#' @export
get_info.glmerMod <- get_info.lmerModLmerTest
#' @export
get_info.lmerMod <- get_info.lmerModLmerTest



#' Get information about models.
#'
#' Get information about models.
#'
#' @param x object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lme4)
#' 
#' fit <- lm(vs ~ wt, data = mtcars, family = "binomial")
#' 
#' info <- get_info(fit)
#' info
#' 
#' #
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_info.lm <- function(x, ...) {
  fit <- x

  info <- tryCatch({

    # Get formula
    formula <- get_formula(fit)
    # Get variables
    predictors <- all.vars(formula)
    outcome <- predictors[[1]]
    predictors <- tail(predictors, -1)

    return(list(
      formula = formula,
      predictors = predictors,
      outcome = outcome
    ))
  }, error = function(e) {

    # Get formula
    formula <- get_formula(fit)
    # Get variables
    predictors <- NA
    outcome <- "Y"
    random <- NA

    return(list(
      formula = formula,
      predictors = predictors,
      outcome = outcome
    ))
  })

  return(info)
}

#' @export
get_info.stanreg <- get_info.lm
#' @export
get_info.lm <- get_info.lm
#' @export
get_info.glm <- get_info.lm
