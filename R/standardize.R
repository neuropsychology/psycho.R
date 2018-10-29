#' Standardize.
#'
#' Standardize objects. See the documentation for your object's class:
#' \itemize{
#' \item{\link[=standardize.numeric]{standardize.numeric}}
#' \item{\link[=standardize.data.frame]{standardize.data.frame}}
#' \item{\link[=standardize.stanreg]{standardize.stanreg}}
#' \item{\link[=standardize.lm]{standardize.lm}}
#' \item{\link[=standardize.glm]{standardize.glm}}
#'  }
#'
#' @param x Object.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
standardize <- function(x, ...) {
  UseMethod("standardize")
}
























#' Standardize (scale and reduce) numeric variables.
#'
#' Standardize (Z-score, "normalize") a vector.
#'
#' @param x Numeric vector.
#' @param normalize Will perform a normalization instead of a standardization. This scales all numeric variables in the range 0 - 1.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' standardize(x=c(1, 4, 6, 2))
#' standardize(x=c(1, 4, 6, 2), normalize=TRUE)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
standardize.numeric <- function(x, normalize = FALSE, ...) {
  if (all(is.na(x)) | length(unique(x)) == 2) {
    return(x)
  }

  if (normalize == FALSE) {
    return(as.vector(scale(x, ...)))
  } else {
    return(as.vector((x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE), na.rm = TRUE)))
  }
}


















#' Standardize (scale and reduce) Dataframe.
#'
#' Selects numeric variables and standardize (Z-score, "normalize") them.
#'
#' @param x Dataframe.
#' @param subset Character or list of characters of column names to be
#' standardized.
#' @param except Character or list of characters of column names to be excluded
#' from standardization.
#' @param normalize Will perform a normalization instead of a standardization. This scales all numeric variables in the range 0 - 1.
#' @param ... Arguments passed to or from other methods.
#'
#' @return Dataframe.
#'
#' @examples
#' \dontrun{
#' df <- data.frame(
#'   Participant = as.factor(rep(1:25,each=4)),
#'   Condition = base::rep_len(c("A", "B", "C", "D"), 100),
#'   V1 = rnorm(100, 30, .2),
#'   V2 = runif(100, 3, 5),
#'   V3 = rnorm(100, 100, 10)
#'   )
#'
#' dfZ <- standardize(df)
#' dfZ <- standardize(df, except="V3")
#' dfZ <- standardize(df, except=c("V1", "V2"))
#' dfZ <- standardize(df, subset="V3")
#' dfZ <- standardize(df, subset=c("V1", "V2"))
#' dfZ <- standardize(df, normalize=TRUE)
#'
#' # Respects grouping
#' dfZ <- df %>%
#'   dplyr::group_by(Participant) %>%
#'   standardize(df)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @importFrom purrr keep discard
#' @import dplyr
#' @export
standardize.data.frame <- function(x, subset = NULL, except = NULL, normalize = FALSE, ...) {
  if (inherits(x, "grouped_df")) {
    dfZ <- x %>% dplyr::do_(".standardize_df(., subset=subset, except=except, normalize=normalize, ...)")
  } else {
    dfZ <- .standardize_df(x, subset = subset, except = except, normalize = normalize, ...)
  }

  return(dfZ)
}

















#' @keywords internal
.standardize_df <- function(x, subset = NULL, except = NULL, normalize = FALSE, ...) {
  df <- x

  # Variable order
  var_order <- names(df)

  # Keep subset
  if (!is.null(subset) && subset %in% names(df)) {
    to_keep <- as.data.frame(df[!names(df) %in% c(subset)])
    df <- df[names(df) %in% c(subset)]
  } else {
    to_keep <- NULL
  }

  # Remove exceptions
  if (!is.null(except) && except %in% names(df)) {
    if (is.null(to_keep)) {
      to_keep <- as.data.frame(df[except])
    } else {
      to_keep <- cbind(to_keep, as.data.frame(df[except]))
    }

    df <- df[!names(df) %in% c(except)]
  }

  # Remove non-numerics
  dfother <- purrr::discard(df, is.numeric)
  dfnum <- purrr::keep(df, is.numeric)

  # Scale
  dfnum <- as.data.frame(sapply(dfnum, standardize, normalize = normalize))

  # Add non-numerics
  if (is.null(ncol(dfother))) {
    df <- dfnum
  } else {
    df <- dplyr::bind_cols(dfother, dfnum)
  }

  # Add exceptions
  if (!is.null(subset) | !is.null(except) && exists("to_keep")) {
    df <- dplyr::bind_cols(df, to_keep)
  }

  # Reorder
  df <- df[var_order]

  return(df)
}













#' Standardize Posteriors.
#'
#' Compute standardized posteriors from which to get standardized coefficients.
#'
#' @param x A stanreg model.
#' @param method "refit" (default) will entirely refit the model based on standardized data. Can take a long time. Other post-hoc methods are "posterior" (based on estimated SD) or "sample" (based on the sample SD).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' fit <- rstanarm::stan_glm(Sepal.Length ~ Sepal.Width * Species, data=iris)
#' fit <- rstanarm::stan_glm(Sepal.Length ~ Sepal.Width * Species, data=standardize(iris))
#' posteriors <- standardize(fit)
#' posteriors <- standardize(fit, method="posterior")
#'
#' }
#'
#' @author \href{https://github.com/jgabry}{Jonah Gabry}, \href{https://github.com/bgoodri}{bgoodri}
#'
#' @seealso https://github.com/stan-dev/rstanarm/issues/298
#'
#' @importFrom utils capture.output
#' @export
standardize.stanreg <- function(x, method = "refit", ...) {
  fit <- x

  predictors <- get_info(fit)$predictors
  predictors <- c("(Intercept)", predictors)

  if (method == "sample") {
    # By jgabry
    predictors <- all.vars(as.formula(fit$formula))
    outcome <- predictors[[1]]
    X <- as.matrix(model.matrix(fit)[, -1]) # -1 to drop column of 1s for intercept
    sd_X_over_sd_y <- apply(X, 2, sd) / sd(fit$data[[outcome]])
    beta <- as.matrix(fit, pars = colnames(X)) # posterior distribution of regression coefficients
    posteriors_std <- sweep(beta, 2, sd_X_over_sd_y, "*") # multiply each row of b by sd_X_over_sd_y
  } else if (method == "posterior") {
    # By bgoordi
    X <- model.matrix(fit)
    # if(preserve_factors == TRUE){
    #   X <- as.data.frame(X)
    #   X[!names(as.data.frame(X)) %in% predictors] <- scale(X[!names(as.data.frame(X)) %in% predictors])
    #   X <- as.matrix(X)
    # }
    sd_X <- apply(X, MARGIN = 2, FUN = sd)[-1]
    sd_Y <- apply(rstanarm::posterior_predict(fit), MARGIN = 1, FUN = sd)
    beta <- as.matrix(fit)[, 2:ncol(X), drop = FALSE]
    posteriors_std <- sweep(
      sweep(beta, MARGIN = 2, STATS = sd_X, FUN = `*`),
      MARGIN = 1, STATS = sd_Y, FUN = `/`
    )
  } else {
    useless_output <- capture.output(fit_std <- update(fit, data = standardize(fit$data)))
    posteriors_std <- as.data.frame(fit_std)
  }

  return(posteriors_std)
}







#' Standardize Coefficients.
#'
#' Compute standardized coefficients.
#'
#' @param x A linear model.
#' @param method The standardization method. Can be "refit" (will entirely refit the model based on standardized data. Can take some time) or "agresti".
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' fit <- glm(Sex ~ Adjusting, data=psycho::affective, family="binomial")
#' fit <- lme4::glmer(Sex ~ Adjusting + (1|Sex), data=psycho::affective, family="binomial")
#'
#' standardize(fit)
#'
#' }
#'
#' @author Kamil Barton
#' @importFrom stats model.frame model.response model.matrix
#'
#' @seealso https://think-lab.github.io/d/205/
#'
#' @export
standardize.glm <- function(x, method = "refit", ...) {
  fit <- x

  if (method == "agresti") {
    coefs <- MuMIn::coefTable(fit)[, 1:2]
    X <- as.matrix(model.matrix(fit)[, -1]) # -1 to drop column of 1s for intercept
    sd_X <- sd(X, na.rm = TRUE)
    coefs <- coefs * sd_X
  } else {
    # refit method
    data <- get_data(fit)
    fit_std <- update(fit, data = standardize(data))


    coefs <- MuMIn::coefTable(fit_std)[, 1:2]
  }

  coefs <- as.data.frame(coefs)
  names(coefs) <- c("Coef_std", "SE_std")
  return(coefs)
}

#' @export
standardize.glmerMod <- standardize.glm



#' Standardize Coefficients.
#'
#' Compute standardized coefficients.
#'
#' @param x A linear model.
#' @param method The standardization method. Can be "refit" (will entirely refit the model based on standardized data. Can take some time) or "posthoc".
#' @param partial_sd Logical, if set to TRUE, model coefficients are multiplied by partial SD, otherwise they are multiplied by the ratio of the standard deviations of the independent variable and dependent variable.
#' @param preserve_factors Standardize factors-related coefs only by the dependent variable (i.e., do not standardize the dummies generated by factors).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' df <- mtcars %>%
#'   mutate(cyl = as.factor(cyl))
#'
#' fit <- lm(wt ~ mpg * cyl, data=df)
#' fit <- lmerTest::lmer(wt ~ mpg * cyl + (1|gear), data=df)
#'
#' summary(fit)
#' standardize(fit)
#'
#' }
#'
#' @author Kamil Barton
#' @importFrom stats model.frame model.response model.matrix
#'
#' @export
standardize.lm <- function(x, method = "refit", partial_sd = FALSE, preserve_factors = TRUE, ...) {
  fit <- x

  if (method == "posthoc") {
    coefs <- .standardize_coefs(fit, partial_sd = partial_sd, preserve_factors = preserve_factors)
  } else {
    data <- get_data(fit)
    fit_std <- update(fit, data = standardize(data))
    coefs <- MuMIn::coefTable(fit_std)[, 1:2]
  }

  coefs <- as.data.frame(coefs)
  names(coefs) <- c("Coef_std", "SE_std")
  return(coefs)
}


#' @export
standardize.lmerMod <- standardize.lm

















#' @keywords internal
.partialsd <-
  function(x, sd, vif, n, p = length(x) - 1) {
    sd * sqrt(1 / vif) * sqrt((n - 1) / (n - p))
  }


#' @importFrom stats vcov
#' @keywords internal
.vif <-
  function(x) {
    v <- vcov(x)
    nam <- dimnames(v)[[1L]]
    if (dim(v)[1L] < 2L) {
      return(structure(rep_len(1, dim(v)[1L]),
        names = dimnames(v)[[1L]]
      ))
    }
    if ((ndef <- sum(is.na(MuMIn::coeffs(x)))) > 0L) {
      stop(sprintf(ngettext(
        ndef, "one coefficient is not defined",
        "%d coefficients are not defined"
      ), ndef))
    }
    o <- attr(model.matrix(x), "assign")
    if (any(int <- (o == 0))) {
      v <- v[!int, !int, drop = FALSE]
    } else {
      warning("no intercept: VIFs may not be sensible")
    }
    d <- sqrt(diag(v))
    rval <- numeric(length(nam))
    names(rval) <- nam
    rval[!int] <- diag(solve(v / (d %o% d)))
    rval[int] <- 1
    rval
  }



#' @importFrom stats nobs vcov
#' @keywords internal
.standardize_coefs <- function(fit, partial_sd = FALSE, preserve_factors = TRUE, ...) {
  # coefs <- MuMIn::coefTable(fit, ...)
  coefs <- as.data.frame(MuMIn::coefTable(fit))
  model_matrix <- model.matrix(fit)

  predictors <- get_info(fit)$predictors
  predictors <- c("(Intercept)", predictors)

  if (preserve_factors == TRUE) {
    response_sd <- sd(model.response(model.frame(fit)))
    factors <- as.data.frame(model_matrix)[!names(as.data.frame(model_matrix)) %in% predictors]
    bx_factors <- rep(1 / response_sd, length(names(factors)))
    bx_factors <- data.frame(t(bx_factors))
    names(bx_factors) <- names(factors)
    coefs_factors <- coefs[names(factors), ]
    model_matrix_factors <- as.matrix(factors)

    coefs <- coefs[!rownames(coefs) %in% names(factors), ]
    model_matrix <- as.matrix(as.data.frame(model_matrix)[names(as.data.frame(model_matrix)) %in% predictors])
  }

  if (partial_sd == TRUE) {
    bx <- .partialsd(
      coefs[, 1L],
      apply(model_matrix, 2L, sd),
      .vif(fit),
      nobs(fit),
      sum(attr(model_matrix, "assign") != 0)
    )
  } else {
    response_sd <- sd(model.response(model.frame(fit)))
    bx <- apply(model_matrix, 2L, sd) / response_sd
  }
  bx <- as.data.frame(t(bx))
  names(bx) <- row.names(coefs)

  if (preserve_factors == TRUE) {
    bx <- cbind(bx, bx_factors)
  }


  # coefs <- MuMIn::coefTable(fit, ...)
  coefs <- as.data.frame(MuMIn::coefTable(fit))
  multiplier <- as.numeric(bx[row.names(coefs)])

  coefs[, 1L:2L] <- coefs[, 1L:2L] * multiplier
  colnames(coefs)[1L:2L] <- c("Coef.std", "SE.std")
  return(coefs)
}
