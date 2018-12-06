#' Interpret fit measures of lavaan or blavaan objects
#'
#' Interpret fit measures of lavaan or blavaan objects
#'
#' @param fit lavaan or blavaan object.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_lavaan <- function(fit, ...) {
  UseMethod("interpret_lavaan")
}







#' @export
interpret_lavaan.lavaan <- function(fit, ...){
  values <- list()

  indices <- lavaan::fitmeasures(fit)


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

  # Summary
  summary <- data.frame(
    Index = c("RMSEA", "CFI", "GFI", "TLI", "NFI", "Chisq"),
    Value = c(values$rmsea, values$cfi, values$gfi, values$tli, values$nfi, values$chisq),
    Interpretation = c(rmsea, cfi, gfi, tli, nfi, NA),
    Treshold = c("< .08", "> .90", "> 0.90", "> 0.90", "> 0.90", NA)
  )

  # Text
  if ("satisfactory" %in% summary$Interpretation) {
    satisfactory <- summary %>%
      filter_("Interpretation == 'satisfactory'") %>%
      mutate_("Index" = "paste0(Index, ' (', format_digit(Value), ' ', Treshold, ')')") %>%
      select_("Index") %>%
      pull() %>%
      paste0(collapse = ", ")
    satisfactory <- paste0("The ", satisfactory, " show satisfactory indices of fit.")
  } else {
    satisfactory <- ""
  }
  if ("poor" %in% summary$Interpretation) {
    poor <- summary %>%
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

  output <- list(text = text, summary = summary, values = values, plot="Not available yet")
  class(output) <- c("psychobject", "list")
  return(output)

}






#' @param indices Vector of strings indicating which indices to report. Only works for bayesian objects for now.
#' @inheritParams interpret_lavaan
#' @export
interpret_lavaan.blavaan <- function(fit, indices=c("BIC", "DIC", "WAIC", "LOOIC"), ...){
  values <- list()

  indices <- lavaan::fitmeasures(fit)


  for (index in names(indices)) {
    values[index] <- indices[index]
  }

  # Summary
  summary <- as.data.frame(indices) %>%
    rownames_to_column("Index") %>%
    rename_("Value" = "indices") %>%
    mutate_("Index" = "str_to_upper(Index)")

  # Text
  relevant_indices <- summary[summary$Index %in% c("BIC", "DIC", "WAIC", "LOOIC"),]
  text <- paste0(relevant_indices$Index, " = ", format_digit(relevant_indices$Value), collapse=", ")

  output <- list(text = text, summary = summary, values = values, plot="Not available yet")
  class(output) <- c("psychobject", "list")
  return(output)

}


