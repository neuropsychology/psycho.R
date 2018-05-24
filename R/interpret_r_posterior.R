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
      "Opposite" = "ifelse(Median == Direction, FALSE, TRUE)"
    )

  values <- list()
  for (strength in c("very strong", "strong", "moderate", "weak", "very weak")) {
    if (strength %in% summary$Strength) {
      values[strength] <- summary[summary$Strength == strength & summary$Opposite == FALSE, ]$Probability
    } else {
      values[strength] <- 0
    }
  }
  values$opposite <- sum(summary[summary$Opposite == TRUE, ]$Probability)

  text <- paste0(
    "This correlation can be considered as very strong, strong, moderate, weak or very weak with respective probabilities of ",
    format_digit(values$`very strong` * 100),
    "%, ",
    format_digit(values$strong * 100),
    "%, ",
    format_digit(values$moderate * 100),
    "%, ",
    format_digit(values$weak * 100),
    "% and ",
    format_digit(values$`very weak` * 100),
    "%."
  )

  plot <- "Not available."

  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")

  return(output)
}
