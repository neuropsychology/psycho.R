#' Analyze htest (correlation, t-test...) objects.
#'
#' Analyze htest (correlation, t-test...) objects.
#'
#' @param x htest object.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_r]{interpret_r}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' 
#' df <- psycho::affective
#' 
#' x <- t.test(df$Tolerating, df$Adjusting)
#' x <- t.test(df$Tolerating ~ df$Sex)
#' x <- t.test(df$Tolerating, mu = 2)
#' x <- cor.test(df$Tolerating, df$Adjusting)
#' 
#' results <- analyze(x)
#' summary(results)
#' print(results)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import dplyr
#'
#' @export
analyze.htest <- function(x, effsize_rules = "cohen1988", ...) {


  # Processing
  # -------------
  values <- list()
  values$method <- x$method
  values$names <- x$data.name
  values$statistic <- x$statistic
  values$effect <- x$estimate
  values$p <- x$p.value
  values$df <- x$parameter
  values$CI <- x$conf.int
  values$signif <- ifelse(values$p < .05, "significant", "not significant")
  values$CI_level <- attr(values$CI, "conf.level") * 100
  values$CI_format <- paste0(values$CI_level, "% CI [", format_digit(values$CI[1]), ", ", format_digit(values$CI[2]), "]")



  # Text
  # -------------

  # CORRELATION
  if (grepl("correlation", values$method)) {
    text <- paste0(
      "The ",
      values$method,
      " between ",
      values$names,
      " is ",
      values$signif,
      ", ",
      interpret_r(values$effect, rules = effsize_rules),
      " (r(",
      format_digit(values$df),
      ") = ",
      format_digit(values$effect),
      ", ",
      values$CI_format,
      ", p ",
      format_p(values$p, stars = FALSE),
      ")."
    )

    # T-TEST
  } else if (grepl("t-test", values$method)) {
    if (names(x$null.value) == "mean") {
      means <- paste0(
        " (mean = ",
        format_digit(values$effect),
        ")"
      )
      vars <- paste0(values$names, means, " and mu = ", x$null.value)
    } else {
      means <- paste0(
        c(
          paste0(
            names(values$effect), " = ",
            format_digit(values$effect)
          ),
          paste0(
            "difference = ",
            format_digit(values$effect[1] - values$effect[2])
          )
        ),
        collapse = ", "
      )
      vars <- paste0(values$names, " (", means, ")")
    }

    values$effect <- values$effect[1] - values$effect[2]

    text <- paste0(
      "The ",
      values$method,
      " suggests that the difference ",
      ifelse(grepl(" by ", values$names), "of ", "between "),
      vars,
      " is ",
      values$signif,
      " (t(",
      format_digit(values$df),
      ") = ",
      format_digit(values$statistic),
      ", ",
      values$CI_format,
      ", p ",
      format_p(values$p, stars = FALSE),
      ")."
    )
    # OTHER
  } else {
    stop(paste0("The ", values$method, " is not implemented yet."))
  }


  # Summary
  # -------------
  summary <- data.frame(
    effect = values$effect,
    statistic = values$statistic,
    df = values$df,
    p = values$p,
    CI_lower = values$CI[1],
    CI_higher = values$CI[2]
  )
  rownames(summary) <- NULL

  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
