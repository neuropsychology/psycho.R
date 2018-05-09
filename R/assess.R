#' Compare a score to a parent population.
#'
#' Compare a given score to a parent population.
#'
#' @param patient Single value (patient's score).
#' @param controls Vector of values (control's scores).
#' @param mean Mean of the control sample.
#' @param sd SD of the control sample.
#' @param n Size of the control sample.
#' @param verbose Print possible warnings.
#' @param CI Credible interval bounds.
#' @param iter Number of iterations.
#' @param color Main color of plot distribution.
#' @param color_CI Color of CI area.
#' @param color_score Color of the line representing the patient's score.
#' @param color_size Size of the line representing the patient's score.
#'
#' @return output
#'
#' @examples
#' result <- assess(patient=124, mean=100, sd=15)
#' print(result)
#' plot(result)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
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
                   verbose=TRUE, CI=95, iter=10000, color="#2196F3", color_CI="#E91E63", color_score="black", color_size=2) {

  if(is.null(n) & is.null(controls)){
    warning("Sample size (n) not provided, thus set to 1000.")
    n <- 1000
  }


  # If score is list
  if (length(patient) > 1) {
    if (verbose == TRUE) {
      warning("Multiple scores were provided. Returning a list of results.")
    }
    results <- list()
    for (i in patient){
      results[i] <- crawford.test(patient,
                                  mean = patient,
                                  sd = sd,
                                  n = n,
                                  controls =controls)
      return(results)
    }
  } else{
    result <- crawford.test(patient,
                            mean = patient,
                            sd = sd,
                            n = n,
                            controls =controls)
    return(result)
  }
}
