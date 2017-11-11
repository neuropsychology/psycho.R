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
          "> .1"))))
}
