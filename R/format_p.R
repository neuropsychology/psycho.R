#' Format p values.
#'
#' @param pvalues P values.
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
format_p <- function(pvalues){
  pvalues <- ifelse(pvalues < 0.001, "< .001***", 
                    ifelse(pvalues < 0.01, "< .01**", 
                           ifelse(pvalues < 0.05, "< .05*", 
                                  ifelse(pvalues < 0.1, paste(round(pvalues, 2), "\xB0", sep=""), "> .1"))))
  return(pvalues)
}
