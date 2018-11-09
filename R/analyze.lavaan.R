#' Analyze lavaan SEM or CFA) objects.
#'
#' Analyze lavaan (SEM or CFA) objects.
#'
#' @param x lavaan object.
#' @param CI Confidence interval level.
#' @param standardize Compute standardized coefs.
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
analyze.lavaan <- function(x, CI=95, standardize=FALSE, ...) {
  fit <- x


  # Processing
  # -------------
  values <- list()
  values$CI = CI

  # Fit measures
  values$Fit_Measures <- interpret_lavaan(fit)




  # Summary
  # -------------
  summary <- .summary_lavaan(fit, CI=CI, standardize=standardize)

  # Plot
  # -------------
  plot <- "Use `get_graph` in association with ggraph."

  output <- list(text = values$Fit_Measures$text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}














#' @keywords internal
.summary_lavaan <- function(fit, CI=95, standardize=FALSE){

  if(standardize == FALSE){
    solution <- lavaan::parameterEstimates(fit, se=TRUE, standardized=standardize, level = CI/100)
  }else{
    solution <- lavaan::standardizedsolution(fit, se=TRUE, level = CI/100) %>%
      rename_("est" = "est.std")
  }

  solution <- solution %>%
    rename("From" = "rhs",
           "To" = "lhs",
           "Operator" = "op",
           "Coef" = "est",
           "SE" = "se",
           "p" = "pvalue",
           "CI_lower" = "ci.lower",
           "CI_higher" = "ci.upper") %>%
    mutate(Type = dplyr::case_when(
      Operator == "=~" ~ "Loading",
      Operator == "~"  ~ "Regression",
      Operator == "~~" ~ "Correlation",
      TRUE ~ NA_character_)) %>%
    mutate_("p" = "replace_na(p, 0)") %>%
    select(one_of(c("From", "Operator", "To", "Coef", "SE", "CI_lower", "CI_higher", "p", "Type")))

  return(solution)
}







