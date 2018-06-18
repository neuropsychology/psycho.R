#' Get Indices of Explanatory Power.
#'
#' See the documentation for your object's class:
#' \itemize{
#' \item{\link[=get_R2.lm]{get_R2.lm}}
#' \item{\link[=get_R2.glm]{get_R2.glm}}
#' \item{\link[=get_R2.stanreg]{get_R2.stanreg}}
#'  }
#'
#' @param fit Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_R2 <- function(fit, ...) {
  UseMethod("get_R2")
}


#' R2 and adjusted R2 for Linear Models.
#'
#' R2 and adjusted R2 for Linear Models.
#'
#' @param fit A linear model.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' fit <- lm(Tolerating ~ Adjusting, data=psycho::affective)
#'
#' get_R2(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @export
get_R2.lm <- function(fit, ...) {
  R2 <- summary(fit)$r.squared
  R2.adj <- summary(fit)$adj.r.squared

  out <- list(R2 = R2, R2.adj = R2.adj)
  return(out)
}



#' Pseudo-R-squared for Logistic Models.
#'
#' Pseudo-R-squared for Logistic Models.
#'
#' @param fit A logistic model.
#' @param method Can be \link[=R2_nakagawa]{"nakagawa"} or \link[=R2_tjur]{"tjur"}.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' fit <- glm(vs ~ wt, data=mtcars, family="binomial")
#' fit <- glm(Sex ~ Adjusting, data=psycho::affective, family="binomial")
#'
#' get_R2(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_R2.glm <- function(fit, method="nakagawa", ...) {
  if (method == "nakagawa") {
    R2 <- as.numeric(R2_nakagawa(fit)$R2m)
  } else if (method == "tjur") {
    R2 <- R2_tjur(fit)
  } else {
    stop("Method must be 'nakagawa' or 'tjur'.")
  }
  return(R2)
}




#' R2 or Bayesian Models.
#'
#' Computes R2 and \link[=R2_LOO_Adjusted]{LOO-adjusted R2}.
#'
#' @param fit A stanreg model.
#' @param silent If R2 not available, throw warning.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' fit <- rstanarm::stan_glm(Adjusting ~ Tolerating, data=psycho::affective)
#'
#' get_R2(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_R2.stanreg <- function(fit, silent=FALSE, ...) {
  tryCatch({
    R2 <- rstanarm::bayes_R2(fit)
  }, error = function(e) {
    R2 <- "NA"
  })

  if (!is.numeric(R2)) {
    if (silent) {
      return(R2)
    } else {
      stop("Couldn't compute R2 for this model.")
    }
  }

  R2.adj <- R2_LOO_Adjusted(fit)

  out <- list(
    R2_median = median(R2),
    R2_MAD = mad(R2),
    R2_posterior = R2,
    R2.adj = R2.adj
  )

  return(out)
}



#' R2 and adjusted R2 for GLMMs.
#'
#' R2 and adjusted R2 for GLMMs.
#'
#' @param fit A GLMM.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1|Sex), data=psycho::affective)
#' fit <- lme4::glmer(Sex ~ Adjusting + (1|Salary), data=na.omit(psycho::affective), family="binomial")
#'
#' get_R2(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @export
get_R2.merMod <- function(fit, ...) {
  out <- suppressMessages(R2_nakagawa(fit))
  return(out)
}





#' Pseudo-R-squared for Generalized Mixed-Effect models.
#'
#' For mixed-effects models, R² can be categorized into two types. Marginal R_GLMM² represents the variance explained by fixed factors, and Conditional R_GLMM² is interpreted as variance explained by both fixed and random factors (i.e. the entire model). IMPORTANT: Looking for help to reimplement this method.
#'
#' @param fit A mixed model.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#' R2_nakagawa(fit)
#'
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @references
#' Nakagawa, S., Johnson, P. C., & Schielzeth, H. (2017). The coefficient of determination R2 and intra-class correlation coefficient from generalized linear mixed-effects models revisited and expanded. Journal of the Royal Society Interface, 14(134), 20170213.
#' Nakagawa, S., & Schielzeth, H. (2013). A general and simple method for obtaining R2 from generalized linear mixed-effects models. Methods in Ecology and Evolution, 4(2), 133-142.
#'
#' @export
R2_nakagawa <- function(fit) {
  out <- MuMIn::r.squaredGLMM(fit)
  out <- list(
    R2m = as.numeric(out[1]),
    R2c = as.numeric(out[2])
  )
  return(out)
}



#' Compute LOO-adjusted R2.
#'
#' Compute LOO-adjusted R2.
#'
#' @param fit A stanreg model.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' data <- attitude
#' fit <- rstanarm::stan_glm(rating ~ advance + privileges, data=data)
#'
#' R2_LOO_Adjusted(fit)
#'
#' }
#'
#' @author \href{https://github.com/strengejacke}{Daniel Luedecke}
#'
#' @import rstantools
#'
#' @export
R2_LOO_Adjusted <- function(fit) {
  predictors <- all.vars(as.formula(fit$formula))
  y <- fit$data[[predictors[[1]]]]
  ypred <- rstantools::posterior_linpred(fit)
  ll <- rstantools::log_lik(fit)

  nsamples <- 0
  nchains <- length(fit$stanfit@stan_args)
  for (chain in fit$stanfit@stan_args) {
    nsamples <- nsamples + (chain$iter - chain$warmup)
  }


  r_eff <- loo::relative_eff(exp(ll),
    chain_id = rep(1:nchains, each = nsamples / nchains)
  )

  psis_object <- loo::psis(log_ratios = -ll, r_eff = r_eff)
  ypredloo <- loo::E_loo(ypred, psis_object, log_ratios = -ll)$value
  eloo <- ypredloo - y

  adj_r_squared <- 1 - stats::var(eloo) / stats::var(y)
  return(adj_r_squared)
}







#' Tjur's (2009) coefficient of determination.
#'
#' Computes Tjur's (2009) coefficient of determination.
#'
#' @param fit Logistic Model.
#'
#' @examples
#' library(psycho)
#' library(lme4)
#'
#' fit <- lme4::glmer(vs ~ wt + (1|gear), data=mtcars, family="binomial")
#' R2_tjur(fit)
#'
#'
#' @author \href{https://github.com/strengejacke}{Daniel Lüdecke}
#'
#' @import dplyr
#' @importFrom stats predict residuals
#' @importFrom lme4 getME
#'
#' @references Tjur, T. (2009). Coefficients of determination in logistic regression models—A new proposal: The coefficient of discrimination. The American Statistician, 63(4), 366-372.
#'
#' @export
R2_tjur <- function(fit) {
  # check for valid object class
  if (!inherits(fit, c("glmerMod", "glm"))) {
    stop("`x` must be an object of class `glm` or `glmerMod`.", call. = F)
  }

  # mixed models (lme4)
  if (inherits(fit, "glmerMod")) {
    # check for package availability
    y <- lme4::getME(fit, "y")
    pred <- stats::predict(fit, type = "response", re.form = NULL)
  } else {
    y <- fit$y
    pred <- stats::predict.glm(fit, type = "response")
  }

  # delete pred for cases with missing residuals
  if (anyNA(stats::residuals(fit))) pred <- pred[!is.na(stats::residuals(fit))]

  categories <- unique(y)
  m1 <- mean(pred[which(y == categories[1])], na.rm = T)
  m2 <- mean(pred[which(y == categories[2])], na.rm = T)

  D <- abs(m2 - m1)
  names(D) <- "Tjur's D"

  return(D)
}
