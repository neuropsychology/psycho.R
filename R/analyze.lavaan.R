#' Analyze lavaan (SEM or CFA) objects.
#'
#' Analyze lavaan (SEM or CFA) objects.
#'
#' @param x lavaan object.
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
  fit <- x


  # Processing
  # -------------
  values <- list()

  # Fit measures
  values$Fit_Measures <- .interpret_fitmeasures(fit)



  # Summary
  # -------------
  summary <- .summary_lavaan(fit)

  # Plot
  # -------------
  plot <- "Use `plot_lavaan` in association with ggraph."

  output <- list(text = values$Fit_Measures$text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}






#' @keywords internal
.interpret_fitmeasures <- function(fit){
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











#' @keywords internal
.summary_lavaan <- function(fit, standardized=TRUE){

  if(standardized == TRUE){
    solution <- lavaan::standardizedSolution(fit)
  } else{
    solution <- lavaan::parameterEstimates(fit)
  }


  solution <- solution %>%
    rename("From" = "rhs",
           "To" = "lhs",
           "Operator" = "op",
           "Coef" = "est.std",
           "SE" = "se",
           "p" = "pvalue",
           "CI_lower" = "ci.lower",
           "CI_higher" = "ci.upper") %>%
    mutate(Type = dplyr::case_when(
      Operator == "=~" ~ "Loading",
      Operator == "~"  ~ "Regression",
      Operator == "~~" ~ "Correlation",
      TRUE ~ NA))

  return(solution)
}







#' Plot lavaan (SEM or CFA) objects.
#'
#' Plot lavaan (SEM or CFA) objects.
#'
#' @param fit lavaan object.
#' @param links Which links to include? A list including at least one of "Regression", "Loading" or "Correlation".
#' @param threshold_p Omit all links with a p value below this value.
#' @param threshold_Coef Omit all links with a Coefs below this value.
#' @param digits Edges' labels rounding.
#'
#' @return A list containing nodes and edges data to be used by `tidygraph::tbl_graph()`.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso
#' https://www.researchgate.net/post/Whats_the_standard_of_fit_indices_in_SEM
#'
#'
#' @export
plot_lavaan <- function(fit, links=c("Regression"), threshold_p=NULL, threshold_Coef=NULL, digits=2){
# https://www.r-bloggers.com/ggplot2-sem-models-with-tidygraph-and-ggraph/

  summary <- summary(analyze(fit))

  # Sanitize
  if(is.null(threshold_p)){
    threshold_p <- 1.1
  }
  if(is.null(threshold_Coef)){
    threshold_Coef <- max(abs(summary$Coef))
  }

  # Edge properties
  edges <- summary %>%
    filter_('Type %in% c(links)',
           "From != To",
           "p < threshold_p",
           "abs(Coef) < threshold_Coef") %>%
    rename_("to" = "To",
            "from" = "From") %>%
    mutate_('Label_Regression' = "ifelse(Type=='Regression', round(Coef, digits), '')",
           'Label_Correlation' = "ifelse(Type=='Correlation', round(Coef, digits), '')",
           'Label_Loading' = "ifelse(Type=='Loading', round(Coef, digits), '')")
  edges <- edges[colSums(!is.na(edges)) > 0]

  # Identify latent variables for nodes
  latent_nodes <- edges %>%
    filter_('Type == "Loading"') %>%
    distinct_("to") %>%
    transmute_("metric" = "to", "latent" = TRUE)

  nodes_list <- unique(c(edges$from, edges$to))

  # Node properties
  nodes <- summary %>%
    filter_("From == To",
            "From %in% nodes_list") %>%
    mutate_("metric" = "From") %>%
    left_join(latent_nodes, by="metric") %>%
    mutate_("latent" = "if_else(is.na(latent), FALSE, latent)")

  return(output = list(nodes = nodes, edges = edges))
}




