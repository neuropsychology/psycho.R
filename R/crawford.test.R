#' Crawford-Howell (1998) t-test for single-case analysis.
#'
#' Neuropsychologists often need to compare a single case to a small control group. However, the standard two-sample t-test does not work because the case is only one observation. Crawford and Garthwaite (2012) demonstrate that the Crawford-Howell (1998) t-test is a better approach (in terms of controlling Type I error rate) than other commonly-used alternatives.
#' .
#'
#' @param case Single value (patient's score).
#' @param controls Vector of values (control's scores).
#' @param verbose True or False. Prints the interpretation text.
#'
#' @return Returns a data frame containing the t-value, degrees of freedom, and p-value. If significant, the case is different from the control group.
#'
#' @examples
#' library(psycho)
#'
#' crawford.test(case = 10, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#' crawford.test(case = 7, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#'
#' @author Dan Mirman, Dominique Makowski
#'
#' @importFrom stats pt sd
#' @export
crawford.test <- function(case, controls, verbose=T) {
  tval <- (case - mean(controls)) / (sd(controls) * sqrt((length(controls) + 1) / length(controls)))

  degfree <- length(controls) - 1

  pval <- 2 * (1 - pt(abs(tval), df = degfree)) # two-tailed p-value

  # One-tailed p value
  if (pval > .05 & pval / 2 < .05) {
    one_tailed <- paste0(
      " However, the null hypothesis of no difference can be rejected at a one-tailed 5% significance level (one-tailed p ",
      format_p(pval / 2),
      ")."
    )
  } else {
    one_tailed <- ""
  }


  p_interpretation <- ifelse(pval < 0.05, " significantly ", " not significantly ")
  t_interpretation <- ifelse(tval < 0, " lower than ", " higher than ")

  text <- paste0(
    "The Crawford-Howell (1998) t-test suggests that the patient's score (",
    format_digit(case),
    ") is",
    p_interpretation,
    "different from the controls (M = ",
    format_digit(mean(controls)),
    ", SD = ",
    format_digit(sd(controls)),
    ", t(",
    degfree,
    ") = ",
    format_digit(tval),
    ", p ",
    format_p(pval),
    ").",
    one_tailed,
    " The patient's score is",
    t_interpretation,
    format_digit((1 - pval) * 100),
    "% of the control population."
  )


  result <- data.frame(t = tval, df = degfree, p = pval)

  if (verbose == T) {
    cat(paste0(text, "\n\n"))
  }

  return(result)
}
