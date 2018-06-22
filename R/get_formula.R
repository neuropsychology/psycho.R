#' Get formula of models.
#'
#' Get formula of models. Implemented for:
#' \itemize{
#'  \item{analyze.merModLmerTest}
#'  \item{analyze.glmerMod}
#'  \item{analyze.lm}
#'  \item{analyze.glm}
#'  \item{analyze.stanreg}
#'  }
#'
#' @param x Object.
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(psycho)
#' library(lme4)
#'
#' fit <- lme4::glmer(vs ~ wt + (1|gear), data=mtcars, family="binomial")
#' fit <- lm(hp ~ wt, data=mtcars)
#'
#' get_formula(fit)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_formula <- function(x, ...) {
  UseMethod("get_formula")
}


#' @export
get_formula.lmerModLmerTest <- function(x, ...) {
  return(x@call$formula)
}
#' @export
get_formula.glmerMod  <- get_formula.lmerModLmerTest



#' @export
get_formula.lm <- function(x, ...) {
  return(stats::formula(x))
}
#' @export
get_formula.glm  <- get_formula.lm



#' @export
get_formula.stanreg <- function(x, ...) {
  return(x$formula)
}

