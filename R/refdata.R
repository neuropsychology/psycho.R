#' Create a reference grid.
#'
#' Create a reference grid.
#'
#' @param df The dataframe.
#' @param target String or list of strings to indicate target columns. Can be "all".
#' @param length.out Length of numeric target variables.
#' @param type "combinations" or "reference". Type of summary for factors.
#' @param fixed String indicating the type of summary for numerics.
#'
#' @examples
#' library(psycho)
#'
#' df <- psycho::affective
#' newdata <- refdata(df, target="Sex")
#' newdata <- refdata(df, target="Sex", type="combinations")
#' newdata <- refdata(df, target=c("Sex", "Salary", "Tolerating"), length.out=3)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom purrr keep
#' @import tidyr
#' @export
refdata <- function(df, target="all", length.out=10, type="reference", fixed="mean") {

  # Target
  if (target == "all" | ncol(df) == 1) {
    return(.refdata_target(df[c(names(df))], length.out = length.out))
  }

  target_df <- .refdata_target(df[c(target)], length.out = length.out)

  # Rest
  df_rest <- select_(df, paste0("-", target))
  if (type == "reference") {
    smart_mean <- function(x) {
      if (is.numeric(x)) {
        out <- mean(x, na.rm = TRUE)
      } else if (is.factor(x)) {
        out <- levels(x)[1]
      } else {
        warning("Argument is not numeric nor factor: returning NA.")
        out <- NA
      }
      return(out)
    }

    refrest <- df_rest %>%
      summarise_all(smart_mean)

    # Join
    refdata <- cbind(target_df, refrest)
  } else {
    var_order <- names(df_rest)
    factors <- purrr::keep(df_rest, is.factor)
    factors_name <- names(factors)

    nums <- purrr::keep(df, is.numeric) %>%
      summarise_all(funs_(fixed))

    factors_df <- tidyr::expand_(df_rest, factors_name)
    refrest <- merge(factors_df, nums)
    refrest <- refrest[var_order]

    # Join
    refdata <- merge(target_df, refrest)
  }

  return(refdata)
}










#' @keywords internal
.refdata_target <- function(target, length.out=10) {
  at_vars <- names(target)
  at_df <- data.frame()
  for (var in at_vars) {
    ref_var <- .refdata_var(target[[var]], length.out = length.out, varname = var)
    if (nrow(at_df) == 0) {
      at_df <- ref_var
    } else {
      at_df <- merge(at_df, ref_var)
    }
  }
  return(at_df)
}


















#' @keywords internal
.refdata_var <- function(x, length.out=10, varname=NULL) {
  if (is.numeric(x)) {
    out <- data.frame(seq(min(x),
      max(x),
      length.out = length.out
    ))
  } else if (is.factor(x)) {
    out <- data.frame(levels(x))
  } else {
    warning("Argument is not numeric nor factor: returning NA.")
    out <- NA
    return()
  }

  if (is.null(varname)) {
    names(out) <- "x"
  } else {
    names(out) <- varname
  }
  return(out)
}
