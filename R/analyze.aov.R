#' Analyze aov objects.
#'
#' Analyze aov objects.
#'
#' @param x aov object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#'
#' df <- psycho::affective
#'
#' x <- aov(df$Tolerating ~ df$Sex)
#'
#' summary(analyze(x))
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import broom
#'
#' @export
analyze.aov <- function(x, ...) {

  # TODO: this must be enhanced!

  # Processing
  # -------------
  values <- broom::tidy(x)

  # Text
  # -------------
  text <- "Not available yet"


  # Summary
  # -------------
  summary <- values

  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
