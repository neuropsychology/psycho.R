#' Format digits.
#'
#' @param x A digit.
#' @param digits Number of significant digits.
#' @param null_treshold Treshold below which return 0.
#' @param inf_treshold Treshold above which return Inf.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_digit <- function(x, digits = 2, null_treshold = 0.001, inf_treshold = 9e+8) {
  if (length(x) > 1) {
    return(sapply(x, .format_digit, digits = digits, null_treshold = null_treshold, inf_treshold = inf_treshold))
  } else {
    return(.format_digit(x, digits = digits, null_treshold = null_treshold, inf_treshold = inf_treshold))
  }
}



#' @keywords internal
.format_digit <- function(x, digits = 2, null_treshold = 0.001, inf_treshold = 9e+8) {

  # if x is NA
  if (is.na(x)) {
    return("NA")
  }

  # if x is inf
  if (x > inf_treshold) {
    return("Inf.")
  }

  # If x is an Integer
  if (all(x == as.integer(x))) {
    formatted <- as.character(x)
  } else {
    # If x is close to zero
    if (abs(x) < null_treshold) {
      formatted <- "0"
    } else {
      # If x is close to trailing zeros
      if (abs(x) < 1) {
        formatted <- as.character(signif(x, digits))
        # If signif cut off trailing zero, add it
        # TODO: that line of code is ugly
        if (nchar(gsub("0|-|\\.", "", formatted)) < digits) {
          formatted <- paste0(formatted, strrep("0", digits - 1))
        }
      } else {
        formatted <- format_string(round(x, digits), paste0("%.", digits, "f"))
      }
    }
  }
  return(formatted)
}


#' Tidyverse-friendly sprintf.
#'
#' @param x Values.
#' @param fmt A character vector of format strings, each of up to 8192 bytes.
#' @param ... values to be passed into fmt. Only logical, integer, real and
#' character vectors are supported, but some coercion will be done: see the ‘Details’ section. Up to 100.
#'
#' @export
format_string <- function(x, fmt, ...) {
  x <- sprintf(fmt, x, ...)
  return(x)
}






#' Format p values.
#'
#' @param pvalues P values (scalar or vector).
#' @param stars Add stars.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stringr str_remove_all
#' @export
format_p <- function(pvalues, stars=TRUE) {
  p <- ifelse(pvalues < 0.001, "< .001***",
    ifelse(pvalues < 0.01, "< .01**",
      ifelse(pvalues < 0.05, "< .05*",
        ifelse(pvalues < 0.1, paste0("= ", round(pvalues, 2), "\xB0"),
          "> .1"
        )
      )
    )
  )

  if (stars == FALSE) {
    p <- stringr::str_remove_all(p, "\\*")
  }

  return(p)
}








#' Clean and format formula.
#'
#' Clean and format formula.
#'
#' @param formula formula
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
#' format_formula(get_formula(fit))
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_formula <- function(formula) {
  formula <- tryCatch({
    stringr::str_squish(paste(format(eval(formula)), collapse = ""))
  }, error = function(e) {
    formula <- stringr::str_squish(paste(format(formula), collapse = ""))
  })

  return(formula)
}
