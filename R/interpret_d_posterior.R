#' Return the effect sizes for the posterior of a standardized coefficient following Cohen (1988).
#'
#' @param posterior Posterior distribution of standardized coefficient.
#'
#' @return The interpretation according to Cohen (1988)
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.6, 0.05)
#' interpret_d_posterior(rnorm(1000, 0.1, 1))
#' interpret_d_posterior(rnorm(1000, 0.6, 0.05))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d_posterior <- function(posterior) {
  summary <- as.data.frame(sapply(posterior, interpret_d, direction = TRUE))
  names(summary) <- "Interpretation"


  summary <- summary %>%
    group_by_("Interpretation") %>%
    summarise_("Probability" = "n() / length(posterior)") %>%
    tidyr::separate("Interpretation",
      c("Size", "Direction"),
      " and ",
      remove = FALSE
    ) %>%
    mutate_(
      "Median" = 'ifelse(median(posterior) > 0, "positive", "negative")',
      "Opposite" = "ifelse(Median == Direction, FALSE, TRUE)",
      "Size" = 'factor(Size, levels=c("very large", "large", "medium", "small", "very small"))'
    ) %>%
    arrange_("Size")

  values <- list()
  for (size in c("very large", "large", "medium", "small", "very small")) {
    if (size %in% summary$Size) {
      if(nrow(summary[summary$Size == size & summary$Opposite == FALSE, ]) == 0){
        values[size] <- 0
      } else{
        values[size] <- summary[summary$Size == size & summary$Opposite == FALSE, ]$Probability
      }
    } else {
      values[size] <- 0
    }
  }
  values$opposite <- sum(summary[summary$Opposite == TRUE, ]$Probability)


  # Text
  if (length(summary[summary$Opposite == FALSE, ]$Size) > 1) {
    text_sizes <- paste0(paste0(head(summary[summary$Opposite == FALSE, ]$Size, -1), collapse = ", "), " or ", tail(summary[summary$Opposite == FALSE, ]$Size, 1))
    text_effects <- paste0(
      paste0(paste0(format_digit(head(summary[summary$Opposite == FALSE, ]$Probability * 100, -1)), "%"), collapse = ", "),
      " and ",
      paste0(format_digit(tail(summary[summary$Opposite == FALSE, ]$Probability, 1) * 100), "%")
    )

    text <- paste0(
      "The effect's size can be considered as ",
      text_sizes,
      " with respective probabilities of ",
      text_effects,
      "."
    )
  } else {
    text_sizes <- summary[summary$Opposite == FALSE, ]$Size
    text_effects <- paste0(format_digit(summary[summary$Opposite == FALSE, ]$Probability * 100), "%")

    text <- paste0(
      "The effect's size can be considered as ",
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
