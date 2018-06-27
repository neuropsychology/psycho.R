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
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' dfZ <- standardize(x=c(1, 4, 6, 2))
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
standardize.numeric <- function(x, ...) {
  return(as.vector(scale(x)))
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
standardize.data.frame <- function(x, subset=NULL, except=NULL, ...) {
  if (inherits(x, "grouped_df")) {
    dfZ <- x %>% dplyr::do_(".standardize_df(., subset=subset, except=except)")
  } else {
    dfZ <- .standardize_df(x, subset = subset, except = except)
  }

  return(dfZ)
}

















#' @keywords internal
.standardize_df <- function(x, subset=NULL, except=NULL) {
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
  dfnum <- as.data.frame(scale(dfnum))

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
#' @param method "posterior" (default, based on estimated SD) or "sample" (based on the sample SD).
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' data <- attitude
#' fit <- rstanarm::stan_glm(rating ~ advance + privileges, data=data)
#'
#' posteriors <- standardize(fit)
#'
#' }
#'
#' @author \href{https://github.com/jgabry}{Jonah Gabry}, \href{https://github.com/bgoodri}{bgoodri}
#' @seealso https://github.com/stan-dev/rstanarm/issues/298
#' @export
standardize.stanreg <- function(x, method="posterior", ...) {
  fit <- x

  if (method == "sample") {
    # By jgabry
    predictors <- all.vars(as.formula(fit$formula))
    outcome <- predictors[[1]]
    X <- as.matrix(model.matrix(fit)[, -1]) # -1 to drop column of 1s for intercept
    sd_X_over_sd_y <- apply(X, 2, sd) / sd(fit$data[[outcome]])
    beta <- as.matrix(fit, pars = colnames(X)) # posterior distribution of regression coefficients
    posteriors_std <- sweep(beta, 2, sd_X_over_sd_y, "*") # multiply each row of b by sd_X_over_sd_y
  } else {
    # By bgoordi
    X <- model.matrix(fit)
    sd_X <- apply(X, MARGIN = 2, FUN = sd)[-1]
    sd_Y <- apply(posterior_predict(fit), MARGIN = 1, FUN = sd)
    beta <- as.matrix(fit)[, 2:ncol(X), drop = FALSE]
    posteriors_std <- sweep(
      sweep(beta, MARGIN = 2, STATS = sd_X, FUN = `*`),
      MARGIN = 1, STATS = sd_Y, FUN = `/`
    )
  }

  return(posteriors_std)
}







#' Standardize Coefficients.
#'
#' Compute standardized coefficients.
#'
#' @param x A linear model.
#' @param method The standardization method. Can be "agresti".
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
standardize.glm <- function(x, method="agresti", ...) {
  fit <- x

  # agresti method
  coefs <- MuMIn::coefTable(fit)[, 1:2]
  X <- as.matrix(model.matrix(fit)[, -1]) # -1 to drop column of 1s for intercept
  sd_X <- sd(X, na.rm = TRUE)
  coefs <- coefs * sd_X

  coefs <- as.data.frame(coefs)
  names(coefs) <- c("Coef.std", "SE.std")
  return(coefs)
}

#' @export
standardize.glmerMod <- standardize.glm



#' Standardize Coefficients.
#'
#' Compute standardized coefficients.
#'
#' @param x A linear model.
#' @param partial_SD Use partial SD.
#' @param ... Arguments passed to or from other methods.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' fit <- lm(Adjusting ~ Tolerating, data=psycho::affective)
#' fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data=iris)
#'
#' standardize(fit)
#'
#' }
#'
#' @author Kamil Barton
#' @importFrom stats model.frame model.response model.matrix
#'
#' @export
standardize.lm <- function(x, partial_SD=TRUE, ...) {
  fit <- x

  coefs <- MuMIn::std.coef(fit, partial.sd = partial_SD)[, 1:2]

  coefs <- as.data.frame(coefs)
  names(coefs) <- c("Coef.std", "SE.std")
  return(coefs)
}


#' @export
standardize.lmerMod <- standardize.lm
