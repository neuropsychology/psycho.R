#' Crawford-Howell (1998) t-test for single-case analysis.
#'
#' Neuropsychologists often need to compare a single case to a small control group. However, the standard two-sample t-test does not work because the case is only one observation. Crawford and Garthwaite (2012) demonstrate that the Crawford-Howell (1998) t-test is a better approach (in terms of controlling Type I error rate) than other commonly-used alternatives.
#' .
#'
#' @param case Single value.
#' @param controls Vector of values.
#' @param verbose True or False. Prints the interpretation text.
#'
#' @return Returns a data frame containing the t-value, degrees of freedom, and p-value. If significant, the case is different from the control group.
#'
#' @examples
#' crawford.test(case = 10, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#'
#' @author Dan Mirman
#'
#' @importFrom stats pt sd
#' @export
crawford.test <- function(case, controls, verbose=T){

  tval <- (case - mean(controls)) / (sd(controls)*sqrt((length(controls)+1) / length(controls)))

  degfree <- length(controls)-1

  pval <- 2*(1-pt(abs(tval), df=degfree)) #two-tailed p-value

  p_interpretation <- ifelse(pval < 0.05, " significantly ", " not ")
  text <- paste0("The Crawford-Howell (1998) t-test suggests that the case partipant is",
                 p_interpretation, "distinct from the control group (t(",
                 degfree,
                 ") = ",
                 format_digit(tval),
                 ", p ",
                 format_p(pval),
                 ").")


  result <- data.frame(t = tval, df = degfree, p=pval)

  if (verbose == T){
    cat(paste0(text, "\n\n"))
  }

  return(result)
}
