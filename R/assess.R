#' Compare a patient's score to a control group
#'
#' Compare a patient's score to a control group.
#'
#' @param patient Single value (patient's score).
#' @param controls Vector of values (control's scores).
#' @param mean Mean of the control sample.
#' @param sd SD of the control sample.
#' @param n Size of the control sample.
#' @param CI Credible interval bounds.
#' @param treshold Significance treshold.
#' @param iter Number of iterations.
#' @param color_controls Color of the controls distribution.
#' @param color_CI Color of CI distribution.
#' @param color_score Color of the line representing the patient's score.
#' @param color_size Size of the line representing the patient's score.
#' @param alpha_controls Alpha of the CI distribution.
#' @param alpha_CI lpha of the controls distribution.
#' @param verbose Print possible warnings.
#'
#' @return output
#'
#' @examples
#' result <- assess(patient = 124, mean = 100, sd = 15, n = 100)
#' print(result)
#' plot(result)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @details Until relatively recently the standard way of testing for a difference between a case and controls was to convert the case’s score to a z score using the control sample mean and standard deviation (SD). If z was less than -1.645 (i.e., below 95% of the controls) then it was concluded that the case was significantly lower than controls. However, this method has serious disadvantages (Crawford and Garthwaite, 2012).
#'
#' @importFrom stats ecdf
#' @import ggplot2
#' @import dplyr
#' @export
assess <- function(patient,
                   mean = 0,
                   sd = 1,
                   n = NULL,
                   controls = NULL,
                   CI = 95,
                   treshold = 0.05,
                   iter = 10000,
                   color_controls = "#2196F3",
                   color_CI = "#E91E63",
                   color_score = "black",
                   color_size = 2,
                   alpha_controls = 1,
                   alpha_CI = 0.8,
                   verbose = TRUE) {
  if (is.null(controls)) {
    if (is.null(n)) {
      if (verbose == TRUE) {
        warning("Sample size (n) not provided, thus set to 1000.")
      }
      n <- 1000
    }
  }




  # If score is list
  if (length(patient) > 1) {
    if (verbose == TRUE) {
      warning("Multiple scores were provided. Returning a list of results.")
    }
    results <- list()
    for (i in seq_len(length(patient))) {
      results[[i]] <- crawford.test(
        patient[i],
        controls,
        mean,
        sd,
        n,
        CI,
        treshold,
        iter,
        color_controls,
        color_CI,
        color_score,
        color_size,
        alpha_controls,
        alpha_CI
      )
      return(results)
    }
  } else {
    result <- crawford.test(
      patient,
      controls,
      mean,
      sd,
      n,
      CI,
      treshold,
      iter,
      color_controls,
      color_CI,
      color_score,
      color_size,
      alpha_controls,
      alpha_CI
    )
    return(result)
  }
}
