#' Compare a score to a parent population.
#'
#' Compare a given score to a parent population.
#'
#' @param score The score.
#' @param mean The general population's mean.
#' @param sd The general population's standart deviation.
#' @param linecolor The colour of the vertical line.
#' @param fillcolor The colour of the density plot.
#' @param verbose Print possible warnings.
#'
#' @return output
#'
#' @examples
#' assess(124, mean=100, sd=15)
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @importFrom stats ecdf
#' @import ggplot2
#' @import dplyr
#' @export
assess <- function(score, mean=0, sd=1, linecolor="#E91E63", fillcolor="#2196F3", verbose=T) {

  # Values
  # -------------
  values <- list()
  values$psycho_function <- "assess"
  values$psycho_name <- deparse(score)

  values$distribution <- stats::rnorm(50000, mean=mean, sd=sd)
  values$percentile <- stats::ecdf(values$distribution)(score)
  values$z_score <- (score-mean)/sd

  # Summary
  # -------------
  summary <- data.frame(Score=score)
  summary$Function <- values$psycho_function
  summary$Percentile <- values$percentile
  summary$Z_Score <- values$z_score


  # If score is list
  if (length(score) > 1){
    if (verbose == T){
      warning('Multiple scores were provided. Returning a list containing summmary and values.')
    }
    output <- list(summary=summary, values=values)
    return(output)
  }


  # Text
  # -------------
  text <- "Text for assess"

  # Plot
  # -------------
  plot <- data.frame(Distribution=values$distribution) %>%
    ggplot(aes_string(x="Distribution")) +
    geom_density(fill=fillcolor, colour="white", adjust = 3, na.rm=TRUE) +
    geom_vline(xintercept=score, size = 2, color=linecolor) +
    xlab("") +
    ylab("")

  output <- list(text=text, plot=plot, summary=summary, values=values)

  class(output) <- c("psycho", "list")
  return(output)
}
