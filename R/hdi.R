#' Highest Density Intervals (HDI).
#'
#' Compute the Highest Density Intervals (HDI) of a distribution.
#'
#' @param x A vector of values from a probability distribution (e.g., posterior probabilities from MCMC sampling).
#' @param prob Scalar between 0 and 1, indicating the mass within the credible interval that is to be estimated.
#'
#' @examples
#' library(psycho)
#'
#' distribution <- rnorm(1000, 0, 1)
#' HDI_values <- HDI(distribution)
#' print(HDI_values)
#' plot(HDI_values)
#' summary(HDI_values)
#'
#' x <- matrix(rexp(200), 100)
#' HDI_values <- HDI(x)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
HDI <- function(x, prob = .95) {

  # From CI to prob if necessary
  if (prob > 1 & prob <= 100) {
    prob <- prob / 100
  }

  # If x is a matrix
  if (ncol(as.matrix(x)) > 1) {
    HDImin <- c()
    HDImax <- c()
    for (col in seq_len(ncol(x))) {
      HDI <- .HDI(x[, col], prob = prob)
      HDImin <- c(HDImin, HDI[1])
      HDImax <- c(HDImax, HDI[2])
    }
    return(data.frame(HDImin = HDImin, HDImax = HDImax))


    # If x is a vector
  } else {
    # Process
    # -------------
    HDI <- .HDI(x, prob = prob)
    HDImin <- HDI[1]
    HDImax <- HDI[2]

    # Store results
    # -------------
    values <- list(HDImin = HDImin, HDImax = HDImax, prob = prob)
    text <- paste(
      prob * 100,
      "% CI [",
      format_string(HDImin, "%.2f"),
      ", ",
      format_string(HDImax, "%.2f"),
      "]",
      sep = ""
    )
    summary <- data.frame(Probability = prob, HDImin = HDImin, HDImax = HDImax)


    # Plot
    # -------------
    data <- as.data.frame(x = x)
    plot <- ggplot(data = data, aes(x)) +
      geom_density(fill = "#2196F3") +
      geom_vline(
        data = data, aes(xintercept = HDImin),
        linetype = "dashed", color = "#E91E63", size = 1
      ) +
      geom_vline(
        data = data, aes(xintercept = HDImax),
        linetype = "dashed", color = "#E91E63", size = 1
      ) +
      theme_minimal()


    # Output
    # -------------
    output <- list(text = text, plot = plot, summary = summary, values = values)

    class(output) <- c("psychobject", "list")
    return(output)
  }
}




#' Highest Density Intervals (HDI)
#'
#' See \link[=HDI]{HDI}
#'
#' @param x A vector of values from a probability distribution (e.g., posterior probabilities from MCMC sampling).
#' @param prob Scalar between 0 and 1, indicating the mass within the credible interval that is to be estimated.
#'
#' @export
HDImin <- function(x, prob = .95) {
  HDImin <- HDI(x, prob = prob)$values$HDImin
  return(HDImin)
}

#' Highest Density Intervals (HDI)
#'
#' See \link[=HDI]{HDI}
#'
#' @param x A vector of values from a probability distribution (e.g., posterior probabilities from MCMC sampling).
#' @param prob Scalar between 0 and 1, indicating the mass within the credible interval that is to be estimated.
#'
#' @export
HDImax <- function(x, prob = .95) {
  HDImax <- HDI(x, prob = prob)$values$HDImax
  return(HDImax)
}






#' @keywords internal
.HDI <- function(x, prob) {
  x <- sort(x)
  ci.index <- ceiling(prob * length(x))
  nCIs <- length(x) - ci.index
  ci.width <- purrr::map_dbl(1:nCIs, ~ x[.x + ci.index] - x[.x])
  HDImin <- x[which.min(ci.width)]
  HDImax <- x[which.min(ci.width) + ci.index]
  return(c(HDImin, HDImax))
}

