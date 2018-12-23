#' Standardized difference (Cohen's d) interpreation.
#'
#' Interpret d with a set of rules.
#'
#' @param x Standardized difference.
#' @param direction Return direction.
#' @param rules Can be "cohen1988" (default), "sawilowsky2009", or a custom list.
#'
#' @examples
#' library(psycho)
#' interpret_d(-0.42)
#' interpret_d(-0.62)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d <- function(x, direction = FALSE, rules = "cohen1988") {
  interpretation <- sapply(x, .interpret_d, direction = direction, rules = rules, return_rules = FALSE)
  return(interpretation)
}







#' Standardized difference (Cohen's d) interpreation for a posterior distribution.
#'
#' Interpret d with a set of rules.
#'
#' @param posterior Posterior distribution of standardized differences.
#' @param rules Can be "cohen1988" (default), "sawilowsky2009", or a custom list.
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.6, 0.05)
#' interpret_d_posterior(posterior)
#' interpret_d_posterior(rnorm(1000, 0.1, 1))
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d_posterior <- function(posterior, rules = "cohen1988") {
  interpretation <- sapply(posterior, .interpret_d, rules = rules, direction = TRUE, return_rules = TRUE)
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
.interpret_d <- function(x, direction = FALSE, rules = "cohen1988", return_rules = TRUE) {
  if (!is.list(rules)) {
    if (rules == "cohen1988") {
      rules <- list(
        "very small" = 0,
        "small" = 0.2,
        "medium" = 0.5,
        "large" = 0.8
      )
    } else if (rules == "sawilowsky2009") {
      rules <- list(
        "tiny" = 0,
        "very small" = 0.1,
        "small" = 0.2,
        "medium" = 0.5,
        "large" = 0.8,
        "very large" = 1.2,
        "huge" = 2.0
      )
    } else {
      stop("rules must be either a list or 'cohen1988' or 'sawilowsky2009'.")
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
