#' Return the interpretation of a posterior distribution of correlation coefficient.
#'
#' @param posterior Posterior distribution of correlation coefficient.
#'
#' @return The interpretation.
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.5, 0.5)
#' interpret_r_posterior(posterior)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_r_posterior <- function(posterior) {
  summary <- as.data.frame(sapply(posterior, interpret_r))
  names(summary) <- "Interpretation"

  summary <- summary %>%
    group_by_("Interpretation") %>%
    summarise_("Probability" = "n() / length(posterior)") %>%
    separate("Interpretation",
      c("Strength", "Direction"),
      " and ",
      remove = FALSE
    ) %>%
    mutate_(
      "Median" = 'ifelse(median(posterior) > 0, "positive", "negative")',
      "Opposite" = "ifelse(Median == Direction, FALSE, TRUE)",
      "Strength" = 'factor(Strength, levels=c("very strong", "strong", "moderate", "weak", "very weak"))'
    ) %>%
    arrange_("Strength")

  values <- list()
  for (strength in c("very strong", "strong", "moderate", "weak", "very weak")) {
    if (strength %in% summary$Strength) {
      values[strength] <- summary[summary$Strength == strength & summary$Opposite == FALSE, ]$Probability
    } else {
      values[strength] <- 0
    }
  }
  values$opposite <- sum(summary[summary$Opposite == TRUE, ]$Probability)

  # Text
  if (length(summary[summary$Opposite == FALSE, ]$Strength) > 1) {
    text_strength <- paste0(paste0(head(summary[summary$Opposite == FALSE, ]$Strength, -1), collapse = ", "), " or ", tail(summary[summary$Opposite == FALSE, ]$Strength, 1))
    text_effects <- paste0(
      paste0(paste0(format_digit(head(summary[summary$Opposite == FALSE, ]$Probability * 100, -1)), "%"), collapse = ", "),
      " and ",
      paste0(format_digit(tail(summary[summary$Opposite == FALSE, ]$Probability, 1) * 100), "%")
    )

    text <- paste0(
      "The correlation can be considered as ",
      text_strength,
      " with respective probabilities of ",
      text_effects,
      "."
    )
  } else {
    text_sizes <- summary[summary$Opposite == FALSE, ]$Strength
    text_effects <- paste0(format_digit(summary[summary$Opposite == FALSE, ]$Probability * 100), "%")

    text <- paste0(
      "The correlation can be considered as ",
      text_sizes,
      " with a probability of ",
      text_effects,
      "."
    )
  }


  plot <- "Not available."

  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")

  return(output)
}
