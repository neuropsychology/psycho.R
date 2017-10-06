#' Analyze merModLmerTest objects.
#'
#' Analyze merModLmerTest objects.
#'
#' @param x merModLmerTest object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @export
analyze.merModLmerTest <- function(x, ...) {


  # Processing
  # -------------
  fit <- x

  # Values
  # -------------
  values <- "Not available yet"



  # Summary
  # -------------
  fitsum <- as.data.frame(summary(fit)$coefficients)

  fitsum$Estimate <- signif(fitsum$Estimate, 2)
  fitsum$`Std. Error` <- signif(fitsum$`Std. Error`, 2)
  fitsum$df <- signif(as.numeric(fitsum$df), 2)
  fitsum$`t value` <- signif(fitsum$`t value`, 2)

  fitsum$Beta <- paste(fitsum$Estimate, "\xB1", fitsum$`Std. Error`)
  fitsum$t <- paste("t(", fitsum$df, ") = ", fitsum$`t value`, sep = "")
  fitsum$p <- format_p(fitsum$`Pr(>|t|)`)

  fitsum <- select_(fitsum, "Beta", "t", "p")
  # Text
  # -------------
  text <- "Not available yet"


  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = fitsum, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
