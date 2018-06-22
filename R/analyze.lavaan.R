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
#' library(lavaan)
#'
#' model <- ' visual  =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed   =~ x7 + x8 + x9 '
#' x <- lavaan::cfa(model, data=HolzingerSwineford1939)
#'
#' rez <- analyze(x)
#' print(rez)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso
#' https://www.researchgate.net/post/Whats_the_standard_of_fit_indices_in_SEM
#'
#'
#' @importFrom lavaan parameterEstimates fitmeasures
#'
#' @export
analyze.lavaan <- function(x, ...) {

  # TODO: this must be enhanced!

  # Processing
  # -------------

  # TODO: tWait for broom to implement methods for lavaan objects!
  values <- list()

  indices <- lavaan::fitmeasures(x)
  for (index in names(indices)) {
    values[index] <- indices[index]
  }

  # awang2012
  # https://www.researchgate.net/post/Whats_the_standard_of_fit_indices_in_SEM
  if (values$cfi >= 0.9) {
    cfi <- "satisfactory"
  } else {
    cfi <- "poor"
  }
  if (values$rmsea <= 0.08) {
    rmsea <- "satisfactory"
  } else {
    rmsea <- "poor"
  }
  if (values$gfi >= 0.9) {
    gfi <- "satisfactory"
  } else {
    gfi <- "poor"
  }
  if (values$tli >= 0.9) {
    tli <- "satisfactory"
  } else {
    tli <- "poor"
  }
  if (values$nfi >= 0.9) {
    nfi <- "satisfactory"
  } else {
    nfi <- "poor"
  }
  fit <- data.frame(
    Index = c("RMSEA", "CFI", "GFI", "TLI", "NFI", "Chisq"),
    Value = c(values$rmsea, values$cfi, values$gfi, values$tli, values$nfi, values$chisq),
    Interpretation = c(rmsea, cfi, gfi, tli, nfi, NA),
    Treshold = c("< .08", "> .90", "> 0.90", "> 0.90", "> 0.90", NA)
  )




  # Text
  # -------------
  if ("satisfactory" %in% fit$Interpretation) {
    satisfactory <- fit %>%
      filter_("Interpretation == 'satisfactory'") %>%
      mutate_("Index" = "paste0(Index, ' (', format_digit(Value), ' ', Treshold, ')')") %>%
      select_("Index") %>%
      pull() %>%
      paste0(collapse = ", ")
    satisfactory <- paste0("The ", satisfactory, " show satisfactory indices of fit.")
  } else {
    satisfactory <- ""
  }
  if ("poor" %in% fit$Interpretation) {
    poor <- fit %>%
      filter_("Interpretation == 'poor'") %>%
      mutate_(
        "Treshold" = 'stringr::str_replace(Treshold, "<", "SUP")',
        "Treshold" = 'stringr::str_replace(Treshold, ">", "INF")',
        "Treshold" = 'stringr::str_replace(Treshold, "SUP", ">")',
        "Treshold" = 'stringr::str_replace(Treshold, "INF", "<")'
      ) %>%
      mutate_("Index" = "paste0(Index, ' (', format_digit(Value), ' ', Treshold, ')')") %>%
      select_("Index") %>%
      pull() %>%
      paste0(collapse = ", ")
    poor <- paste0("The ", poor, " show poor indices of fit.")
  } else {
    poor <- ""
  }
  text <- paste(satisfactory, poor)


  # Summary
  # -------------
  summary <- lavaan::parameterEstimates(x) %>%
    as_data_frame() %>%
    tibble::rownames_to_column() %>%
    mutate_("term" = "paste(lhs, op, rhs)") %>%
    rename(
      "estimate" = "est",
      "Operator" = "op",
      "std.error" = "se",
      "p.value" = "pvalue",
      "statistic" = "z",
      "CI_lower" = "ci.lower",
      "CI_upper" = "ci.upper"
    ) %>%
    select_("term", "Operator", "everything()", "-rowname", "-lhs", "-rhs")

  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
