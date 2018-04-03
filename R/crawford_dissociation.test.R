#' Crawford-Howell (1998) modified t-test for testing difference between a patient’s performance on two tasks.
#'
#' Assessing dissociation between processes is a fundamental part of clinical neuropsychology. However, while the detection of suspected impairments is a fundamental feature of single-case studies, evidence of an impairment on a given task usually becomes of theoretical interest only if it is observed in the context of less impaired or normal performance on other tasks. Crawford and Garthwaite (2012) demonstrate that the Crawford-Howell (1998) t-test for dissociation is a better approach (in terms of controlling Type I error rate) than other commonly-used alternatives.
#' .
#'
#' @param case_X Single value (patient's score on test X).
#' @param case_Y Single value (patient's score on test Y).
#' @param controls_X Vector of values (control's scores of X).
#' @param controls_Y Vector of values (control's scores of Y).
#' @param verbose True or False. Prints the interpretation text.
#'
#' @return Returns a data frame containing the t-value, degrees of freedom, and p-value. If significant, the dissociation between test X and test Y is significant.
#'
#' @examples
#' library(psycho)
#'
#' case_X <- 142
#' case_Y <- 7
#' controls_X <- c(100, 125, 89, 105, 109, 99)
#' controls_Y <- c(7, 8, 9, 6, 7, 10)
#'
#' crawford_dissociation.test(case_X, case_Y, controls_X, controls_Y)
#'
#' @author Dominique Makowski
#'
#' @importFrom stats sd pt
#' @export
crawford_dissociation.test <- function(case_X, case_Y, controls_X, controls_Y, verbose=T) {
  X_mean <- mean(controls_X)
  X_sd <- sd(controls_X)
  Y_mean <- mean(controls_Y)
  Y_sd <- sd(controls_Y)
  r <- cor(controls_X, controls_Y)
  n <- length(controls_X)
  degfree <- n - 1

  case_X_Z <- (case_X - X_mean) / X_sd
  case_Y_Z <- (case_Y - Y_mean) / Y_sd

  tval <- (case_X_Z - case_Y_Z) / sqrt((2 - 2 * r) * ((n + 1) / n))

  pval <- 2 * (1 - pt(abs(tval), df = degfree)) # two-tailed p-value





  p_interpretation <- ifelse(pval < 0.05, " a significant ", " no ")
  p_interpretation2 <- ifelse(pval < 0.05, " ", " not ")
  z_interpretation <- ifelse(tval < 0, " below ", " above ")
  pop_interpretation <- ifelse(tval < 0, " above ", " below ")

  if (abs(case_X_Z) > abs(case_Y_Z)) {
    var_interpretation1 <- "test X"
    var_interpretation2 <- "test Y"
  } else {
    var_interpretation1 <- "test Y"
    var_interpretation2 <- "test X"
  }

  text <- paste0(
    "The Crawford-Howell (1998) t-test suggests",
    p_interpretation,
    "dissociation between test X and test Y (t(",
    degfree,
    ") = ",
    format_digit(tval),
    ", p ",
    format_p(pval),
    "). The patient's score on ",
    var_interpretation1,
    " is",
    p_interpretation2,
    "significantly altered compared to its score on ",
    var_interpretation2,
    "."
  )


  result <- data.frame(t = tval, df = degfree, p = pval)

  if (verbose == T) {
    cat(paste0(text, "\n\n"))
  }

  return(result)
}
