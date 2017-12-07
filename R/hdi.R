#' Highest Density Intervals (HDI).
#'
#' Compute the Highest Density Intervals (HDI) of a distribution.
#'
#' @param x A vector of values from a probability distribution (e.g., posterior probabilities from MCMC sampling).
#' @param prob Scalar between 0 and 1, indicating the mass within the credible interval that is to be estimated.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
hdi <- function(x, prob = .95) {

  # Process
  x <- sort(x)
  ci.index <- ceiling(prob * length(x))
  nCIs <- length(x) - ci.index
  ci.width <- purrr::map_dbl(1:nCIs, ~ x[.x + ci.index] - x[.x])
  HDImin <- x[which.min(ci.width)]
  HDImax <- x[which.min(ci.width) + ci.index]

  # Store results
  values <- list(HDImin = HDImin, HDImax = HDImax, prob=prob)
  text <- paste(prob*100, "% CI [", format_string(HDImin, "%.2f"), ", ", format_string(HDImax, "%.2f"), "]", sep="")
  summary <- data.frame(Probability=prob, HDImin=HDImin, HDImax=HDImax)


  # Plot
  data <- as.data.frame(x=x)
  plot <- ggplot(data=data, aes(x)) +
    geom_density(fill="#2196F3") +
    geom_vline(data=data, aes(xintercept=HDImin),
               linetype="dashed", color="#E91E63", size=1) +
    geom_vline(data=data, aes(xintercept=HDImax),
               linetype="dashed", color="#E91E63", size=1) +
    theme_minimal()


  # Output
  # -------------
  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
