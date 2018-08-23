#' Extract the dataframe used in a model.
#'
#' Extract the dataframe used in a model.
#'
#' @param fit A model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(psycho)
#'
#' df <- mtcars %>%
#'   mutate(cyl = as.factor(cyl),
#'   gear = as.factor(gear))
#'
#' fit <- lm(wt ~ mpg , data=df)
#' fit <- lm(wt ~ cyl, data=df)
#' fit <- lm(wt ~ mpg * cyl, data=df)
#' fit <- lm(wt ~ cyl * gear, data=df)
#' fit <- lmerTest::lmer(wt ~ mpg * gear + (1|cyl), data=df)
#' fit <- rstanarm::stan_lmer(wt ~ mpg * gear + (1|cyl), data=df)
#'
#' get_data(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @export
get_data <- function(fit, ...) {
UseMethod("get_data")
}


#' @importFrom stats getCall
#' @importFrom utils data
#' @export
get_data.lm <- function(fit, ...) {

  tryCatch({
    data <- eval(getCall(fit)$data, environment(formula(fit)))
    return(data)
  })

  info <- get_info(fit)

  outcome <- info$outcome
  predictors <- info$predictors

  data <- as.data.frame(model.frame(fit))


  effects <- names(MuMIn::coeffs(fit))
  effects <- unique(unlist(stringr::str_split(effects, ":")))
  numerics <- predictors[predictors %in% effects]

  numerics <- numerics[!is.na(numerics)]
  if (length(unique(model.response(model.frame(fit)))) > 2) {
    numerics <- c(outcome, numerics)
  }


  data[!names(data) %in% numerics] <- lapply(data[!names(data) %in% numerics], as.factor)
  data[names(data) %in% numerics] <- lapply(data[names(data) %in% numerics], as.numeric)

  return(as.data.frame(data))
}

#' @export
get_data.merMod <- get_data.lm





#' @export
get_data.stanreg <- function(fit, ...) {
  data <- fit$data
  return(data)
}
