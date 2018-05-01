#' Format digits.
#'
#' @param x A digit.
#' @param digits Number of significant digits.
#' @param null_treshold Treshold below which return 0.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_digit <- function(x, digits = 2, null_treshold = 0.001) {
  if(length(x) > 1){
    return(sapply(x, .format_digit, digits=digits, null_treshold=null_treshold))
  } else{
    return(.format_digit(x, digits=digits, null_treshold=null_treshold))
  }
}




#' @keywords internal
.format_digit <- function(x, digits = 2, null_treshold = 0.001) {

  # If x is an Integer
  if (x %% 1 == 0) {
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
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_p <- function(pvalues) {
  ifelse(pvalues < 0.001, "< .001***",
         ifelse(pvalues < 0.01, "< .01**",
                ifelse(pvalues < 0.05, "< .05*",
                       ifelse(pvalues < 0.1, paste0("= ", round(pvalues, 2), "\xB0"),
                              "> .1"
                       )
                )
         )
  )
}

