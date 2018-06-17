#' Omega Squared Interpretation
#'
#' Return the interpretation of Omegas Squared.
#'
#' @param x Odds ratio.
#' @param log Are these log odds ratio?
#' @param direction Return direction.
#' @param rules Can be "chen2010" (default), or a custom list.
#'
#' @examples
#' library(psycho)
#' interpret_odds(x=2)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize
#'
#' @references
#' \itemize{
#'  \item{Chen, H., Cohen, P., & Chen, S. (2010). How big is a big odds ratio? Interpreting the magnitudes of odds ratios in epidemiological studies. Communications in Statistics—Simulation and Computation®, 39(4), 860-864.}
#'  }
#' @export
interpret_odds <- function(x, log=FALSE, direction=FALSE, rules="chen2010") {
  interpretation <- sapply(x, .interpret_odds, log=log, direction=direction, rules = rules, return_rules = FALSE)
  return(interpretation)
}





#' Odds ratio interpreation for a posterior distribution.
#'
#' Interpret odds with a set of rules.
#'
#' @param posterior Posterior distribution of odds ratio.
#' @param log Are these log odds ratio?
#' @param rules Can be "chen2010" (default), or a custom list.
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.6, 0.05)
#' interpret_odds_posterior(posterior)
#' interpret_odds_posterior(rnorm(1000, 0.1, 1))
#' interpret_odds_posterior(rnorm(1000, 3, 1.5))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_odds_posterior <- function(posterior, log=FALSE, rules="chen2010") {
  interpretation <- sapply(posterior, .interpret_odds, log=log, direction=TRUE, rules = rules, return_rules = TRUE)
  rules <- unlist(interpretation[, 1]$rules)
  interpretation <- as.data.frame(unlist(interpretation[1, ]))
  interpretation <- na.omit(interpretation)
  names(interpretation) <- "Interpretation"

  summary <- interpretation %>%
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
      "Size" = "factor(Size)"
    ) %>%
    arrange_("Size")

  values <- list()
  for (size in names(sort(rules, decreasing = TRUE))) {
    if (size %in% summary$Size) {
      if (nrow(summary[summary$Size == size & summary$Opposite == FALSE, ]) == 0) {
        values[size] <- 0
      } else {
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








#' @keywords internal
.interpret_odds <- function(x, log=FALSE, direction=FALSE, rules="chen2010", return_rules=TRUE) {

  if (x > 0) {
    d <- "positive"
  } else {
    d <- "negative"
  }

  if(log == TRUE){
    x <- exp(abs(x))
  }

  if (!is.list(rules)) {
    if (rules == "chen2010") {
      rules <- list(
        "very small" = 0,
        "small" = 1.68,
        "medium" = 3.47,
        "large" = 6.71
      )
    } else {
      stop("rules must be either a list or 'field2013'.")
    }
  }


  s <- (abs(x) - unlist(rules))
  s <- names(which.min(s[s >= 0]))
  if (is.null(s)) {
    s <- NA
  }

  if (direction) {
    interpretation <- paste(s, "and", d)
  } else {
    interpretation <- s
  }

  if (return_rules) {
    return(list(interpretation = interpretation, rules = rules))
  } else {
    return(interpretation)
  }
}
