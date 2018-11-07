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
.interpret_fitmeasures.blavaan <- function(fit, indices=c("BIC", "DIC", "WAIC", "LOOIC")){
  values <- list()

  indices <- fitmeasures(fit)


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



#' @keywords internal
.get_info_computations.blavaan <- function(fit) {
  chains <- blavInspect(fit, "n.chains")
  sample = fit@external$sample
  warmup = fit@external$burnin
  text = paste0(chains,
                "chains, each with iter = ",
                sample,
                "; warmup = ",
                warmup)
  return(text)
}




#' @keywords internal
.process_blavaan <- function(fit, CI=90){
  # Get relevant rows
  PE <- parameterEstimates(fit, se = FALSE, ci=FALSE, remove.eq = FALSE, remove.system.eq = TRUE,
                           remove.ineq = FALSE, remove.def = FALSE,
                           add.attributes = TRUE)
  if(!("group" %in% names(PE))) PE$group <- 1
  newpt <- fit@ParTable
  pte2 <- which(newpt$free > 0)
  relevant_rows <- match(with(newpt, paste(lhs[pte2], op[pte2], rhs[pte2], group[pte2], sep="")),
                   paste(PE$lhs, PE$op, PE$rhs, PE$group, sep=""))

  # Priors
  priors <- rep(NA, nrow(PE))
  priors[relevant_rows] <- newpt$prior[pte2]
  priors[is.na(PE$prior)] <- ""

  # Posterior
  posteriors <- blavInspect(fit, "draws") %>%
    as.matrix() %>%
    as.data.frame()
  names(posteriors) <- names(coef(fit))


  # Effects
  MPE <- c()
  Median <- c()
  MAD <- c()
  Effect <- c()
  CI_lower <- c()
  CI_higher <- c()
  for(effect in names(posteriors)){
    posterior <- posteriors[[effect]]
    Effect <- c(Effect, effect)
    MPE <- c(MPE, mpe(posterior)$MPE)
    Median <- c(Median, median(posterior))
    MAD <- c(MAD, mad(posterior))

    CI_values <- HDI(posterior, prob = CI / 100)
    CI_lower <- c(CI_lower, CI_values$values$HDImin)
    CI_higher <- c(CI_higher, CI_values$values$HDImax)

  }

  Effects <- rep(NA, nrow(PE))
  Effects[relevant_rows] <- Effect
  MPEs <- rep(NA, nrow(PE))
  MPEs[relevant_rows] <- MPE
  Medians <- rep(NA, nrow(PE))
  Medians[relevant_rows] <- Median
  MADs <- rep(NA, nrow(PE))
  MADs[relevant_rows] <- MAD
  CI_lowers <- rep(NA, nrow(PE))
  CI_lowers[relevant_rows] <- CI_lower
  CI_highers <- rep(NA, nrow(PE))
  CI_highers[relevant_rows] <- CI_higher

  data <- data.frame("Effect" = Effects,
                     "Median" = Medians,
                     "MAD" = MADs,
                     "MPE" = MPEs,
                     "CI_lower" = CI_lowers,
                     "CI_higher" = CI_highers,
                     "Prior" = priors)

  return(data)
}



#' @keywords internal
.summary_lavaan <- function(fit){

  solution <- parameterEstimates(fit, se = TRUE, ci=TRUE, standardized=TRUE)


  solution <- solution %>%
    rename("From" = "rhs",
           "To" = "lhs",
           "Operator" = "op",
           "Coef" = "est",
           "Coef_std" = "std.all",
           "SE" = "se",
           "CI_lower" = "ci.lower",
           "CI_higher" = "ci.upper") %>%
    mutate(Type = dplyr::case_when(
      Operator == "=~" ~ "Loading",
      Operator == "~"  ~ "Regression",
      Operator == "~~" ~ "Correlation",
      TRUE ~ NA_character_)) %>%
    select(one_of(c("To", "Operator", "From", "Coef_std", "Type"))) %>%
    cbind(.process_blavaan(fit)) %>%
    select_("-Effect") %>%
    mutate_("Median" = "replace_na(Median, 1)",
            "MAD" = "replace_na(MAD, 0)",
            "MPE" = "replace_na(MPE, 100)") %>%
    select(one_of(c("To", "Operator", "From", "Median", "MAD", "CI_lower", "CI_higher", "MPE", "Coef_std", "Prior", "Type")))


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
           "abs(Coef) > threshold_Coef") %>%
    rename_("to" = "To",
            "from" = "From") %>%
    mutate_('Label_Regression' = "ifelse(Type=='Regression', format_digit(Coef, digits), '')",
           'Label_Correlation' = "ifelse(Type=='Correlation', format_digit(Coef, digits), '')",
           'Label_Loading' = "ifelse(Type=='Loading', format_digit(Coef, digits), '')")
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




