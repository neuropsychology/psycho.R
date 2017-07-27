#' Compare a score to a parent population.
#'
#' Compare a given score to a parent population.
#'
#' @param score The score.
#' @param mean The general population's mean.
#' @param sd The general population's standart deviation.
#' @param linecolor The colour of the vertical line.
#' @param fillcolor The colour of the density plot.
#'
#' @return output
#'
#' @examples
#' assess(124, mean=100, sd=15)
#'
#' @importFrom stats ecdf
#' @import ggplot2
#' @import dplyr
#' @export
assess <- function(score, mean=0, sd=1, linecolor="#E91E63", fillcolor="#2196F3") {
  # Values
  # -------------
  values <- list()
  values$percentile <- stats::ecdf(stats::rnorm(50000, mean=mean, sd=sd))(score)

  # Text
  # -------------
  text <- "Text for assess"

  # Plot
  # -------------
  plot <- stats::rnorm(50000, mean=mean, sd=sd) %>%
    data.frame() %>%
    rename("Parent Distribution" = ".") %>%
    ggplot(aes_(x="Parent Distribution")) +
    geom_density(fill=fillcolor, colour="white", adjust = 3, na.rm=TRUE) +
    geom_vline(xintercept=score, size = 2, color=linecolor) +
    xlab("") +
    ylab("")

  output <- list(text=text, plot=plot, values=values)

  class(output) <- c("psycho", "list")
  return(output)
}

