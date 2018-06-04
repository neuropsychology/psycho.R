#' Correlation coefficient r interpreation.
#'
#' Interpret r with a set of rules.
#'
#' @param x Correlation coefficient.
#' @param direction Return direction.
#' @param strength Return strength.
#' @param rules Can be "cohen1988" (default), "evans1996", or a custom list.
#'
#'
#' @examples
#' library(psycho)
#' interpret_r(-0.42)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_r <- function(x, direction=TRUE, strength=TRUE, rules="cohen1988") {
  interpretation <- sapply(x, .interpret_r, direction = direction, strength = strength, rules = rules, return_rules = FALSE)
  return(interpretation)
}









#' Correlation coefficient r interpreation for a posterior distribution.
#'
#' Interpret r with a set of rules.
#'
#' @param posterior Posterior distribution of correlation coefficient.
#' @param rules Can be "cohen1988" (default) or "evans1996", or a custom list.
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.5, 0.5)
#' interpret_r_posterior(posterior)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_r_posterior <- function(posterior, rules="cohen1988") {
  interpretation <- sapply(posterior, .interpret_r, rules = rules)
  rules <- unlist(interpretation[, 1]$rules)
  interpretation <- as.data.frame(unlist(interpretation[1, ]))
  interpretation <- na.omit(interpretation)
  names(interpretation) <- "Interpretation"

  summary <- interpretation %>%
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
      "Strength" = "factor(Strength)"
    ) %>%
    arrange_("Strength")

  values <- list()
  for (strength in names(sort(rules, decreasing = TRUE))) {
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
















#' @keywords internal
.interpret_r <- function(x, direction=TRUE, strength=TRUE, rules="cohen1988", return_rules=TRUE) {
  if (!is.list(rules)) {
    if (rules == "evans1996") {
      rules <- list(
        "very weak" = 0,
        "weak" = 0.20,
        "moderate" = 0.40,
        "strong" = 0.60,
        "very strong" = 0.80
      )
    } else if (rules == "cohen1988") {
      rules <- list(
        "very small" = 0,
        "small" = 0.10,
        "moderate" = 0.30,
        "large" = 0.50
      )
    } else {
      stop("rules must be either a list or 'cohen1988' or 'evans1996'.")
    }
  }


  if (x > 0) {
    d <- "positive"
  } else {
    d <- "negative"
  }

  x <- (abs(x) - unlist(rules))
  s <- names(which.min(x[x >= 0]))
  if (is.null(s)) {
    s <- NA
  }



  if (strength & direction) {
    interpretation <- paste(s, "and", d)
  } else if (strength & direction == FALSE) {
    interpretation <- s
  } else {
    interpretation <- d
  }



  if (return_rules) {
    return(list(interpretation = interpretation, rules = rules))
  } else {
    return(interpretation)
  }
}
