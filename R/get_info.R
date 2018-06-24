#' Get information about objects.
#'
#' Get information about objects. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=analyze.lmerModLmerTest]{analyze.merModLmerTest}}
#'  \item{\link[=analyze.glmerMod]{analyze.glmerMod}}
#'  \item{\link[=analyze.lm]{analyze.lm}}
#'  \item{\link[=analyze.lm]{analyze.stanreg}}
#'  }
#'  \itemize{
#'  \item{\link[=analyze.htest]{analyze.htest}}
#'  \item{\link[=analyze.aov]{analyze.aov}}
#'  }
#' \itemize{
#'  \item{\link[=analyze.fa]{analyze.fa}}
#'  \item{\link[=analyze.fa]{analyze.lavaan}}
#'  }
#'
#' @param x Object.
#' @param ... Arguments passed to or from other methods.
#'
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
#' fit <- lme4::glmer(vs ~ wt + (1|gear), data=mtcars, family="binomial")
#'
#' info <- get_info(fit)
#' info
#'
#
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

    return(list(
      formula = formula,
      predictors = predictors,
      outcome = outcome
    ))
  })

  return(info)
}

#' @export
get_info.glmerMod <- get_info.lmerModLmerTest
#' @export
get_info.lmerMod <- get_info.lmerModLmerTest
#' @export
get_info.stanreg <- get_info.lmerModLmerTest
#' @export
get_info.lm <- get_info.lmerModLmerTest
#' @export
get_info.glm <- get_info.lmerModLmerTest
