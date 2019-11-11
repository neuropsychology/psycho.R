

#' R2 interpreation for a posterior distribution.
#'
#' Interpret R2 with a set of rules.
#'
#' @param posterior Distribution of R2.
#' @param rules Can be "cohen1988" (default), "chin1998" or "hair2013", or a custom list.
#'
#' @examples
#' library(psycho)
#' posterior <- rnorm(1000, 0.4, 0.1)
#' interpret_R2_posterior(posterior)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @importFrom stats na.omit
#' @importFrom utils head tail
#' @export
interpret_R2_posterior <- function(posterior, rules = "cohen1988") {
  interpretation <- sapply(posterior, .interpret_R2, rules = rules)
  rules <- unlist(interpretation[, 1]$rules)
  interpretation <- as.data.frame(unlist(interpretation[1, ]))
  interpretation <- na.omit(interpretation)
  names(interpretation) <- "Interpretation"

  summary <- interpretation %>%
    group_by_("Interpretation") %>%
    summarise_("Probability" = "n() / length(posterior)")

  values <- list()
  for (value in names(sort(rules, decreasing = TRUE))) {
    if (value %in% summary$Interpretation) {
      values[value] <- summary[summary$Interpretation == value, ]$Probability
    } else {
      values[value] <- 0
    }
  }

  # Text
  if (length(summary$Interpretation) > 1) {
    text_strength <- paste0(paste0(head(summary$Interpretation, -1), collapse = ", "), " or ", tail(summary$Interpretation, 1))
    text_effects <- paste0(
      paste0(paste0(insight::format_value(head(summary$Probability * 100, -1)), "%"), collapse = ", "),
      " and ",
      paste0(insight::format_value(tail(summary$Probability, 1) * 100), "%")
    )

    text <- paste0(
      "The R2 can be considered as ",
      text_strength,
      " with respective probabilities of ",
      text_effects,
      "."
    )
  } else {
    text_sizes <- summary$Interpretation
    text_effects <- paste0(insight::format_value(summary$Probability * 100), "%")

    text <- paste0(
      "The R2 can be considered as ",
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
.interpret_R2 <- function(x, rules = "cohen1988", return_rules = TRUE) {
  if (!is.list(rules)) {
    if (rules == "cohen1988") {
      rules <- list(
        "very small" = 0,
        "small" = 0.02,
        "medium" = 0.13,
        "large" = 0.26
      )
    } else if (rules == "chin1998") {
      rules <- list(
        "very small" = 0,
        "small" = 0.19,
        "medium" = 0.33,
        "large" = 0.67
      )
    } else if (rules == "hair2013") {
      rules <- list(
        "very small" = 0,
        "small" = 0.25,
        "medium" = 0.50,
        "large" = 0.75
      )
    } else {
      stop("rules must be either a list or 'cohen1988', 'chin1998' or 'hair2013'.")
    }
  }

  x <- (x - unlist(rules))
  interpretation <- names(which.min(x[x >= 0]))
  if (is.null(interpretation)) {
    interpretation <- NA
  }

  if (return_rules) {
    return(list(interpretation = interpretation, rules = rules))
  } else {
    return(interpretation)
  }
}


