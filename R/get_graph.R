#' Get graph data.
#'
#' To be used with tidygraph::tbl_graph. See the documentation for your object's class:
#' \itemize{
#'  \item{\link[=get_graph.lavaan]{get_graph.lavaan}}
#'  \item{\link[=get_graph.fa]{get_graph.fa}}
#'  \item{\link[=get_graph.psychobject_correlation]{get_graph.psychobject_correlation}}
#'  }
#'
#' @param fit Object from which to extract the graph data.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
get_graph <- function(fit, ...) {
  UseMethod("get_graph")
}


















#' Get graph data from lavaan or blavaan objects.
#'
#' Get graph data from lavaan or blavaan objects.
#'
#' @param fit lavaan object.
#' @param links Which links to include? A list including at least one of "Regression", "Loading" or "Correlation".
#' @param standardize Use standardized coefs.
#' @param threshold_Coef Omit all links with a Coefs below this value.
#' @param threshold_p Omit all links with a p value above this value.
#' @param threshold_MPE In case of a blavaan model, omit all links with a MPE value below this value.
#' @param digits Edges' labels rounding.
#' @param CI CI level.
#' @param labels_CI Add the CI in the edge label.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A list containing nodes and edges data to be used by `tidygraph::tbl_graph()`.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
get_graph.lavaan <- function(fit, links=c("Regression", "Correlation", "Loading"), standardize=FALSE, threshold_Coef=NULL, threshold_p=NULL, threshold_MPE=NULL, digits=2, CI="default", labels_CI=TRUE, ...){
  # https://www.r-bloggers.com/ggplot2-sem-models-with-tidygraph-and-ggraph/


  if(labels_CI==TRUE){
    if(CI != "default"){
      results <- analyze(fit, CI=CI, standardize=standardize)
    } else{
      results <- analyze(fit, standardize=standardize)
    }
  } else{
    results <- analyze(fit, standardize=standardize)
  }

  summary <- summary(results)
  CI <- results$values$CI

  # Check what type of model
  if(class(fit) %in% c("blavaan")){
    summary$Coef <- summary$Median
    if(is.null(threshold_MPE)){
      threshold_MPE <- -1
    }
    summary <- summary %>%
      filter_("MPE >= threshold_MPE")

  } else if(class(fit) %in% c("lavaan")){
    if(is.null(threshold_p)){
      threshold_p <- 1.1
    }
    summary <- summary %>%
      filter_("p <= threshold_p")
  } else{
    stop(paste("Error in UseMethod('plot_lavaan') : no applicable method for 'plot_lavaan' applied to an object of class", class(fit)))
  }

  # Deal with thresholds
  if(is.null(threshold_Coef)){
    threshold_Coef <- min(abs(summary$Coef))-1
  }

  # Edge properties
  edges <- summary %>%
    mutate_("abs_coef" = "abs(Coef)") %>%
    filter_('Type %in% c(links)',
            "From != To",
            "abs_coef >= threshold_Coef") %>%
    select(-one_of("abs_coef")) %>%
    rename_("to" = "To",
            "from" = "From")

  # Labels
  if(labels_CI == TRUE){
    edges <- edges %>%
      mutate_('Label' = 'paste0(format_digit(Coef, digits),
              ", ", CI, "% CI [", format_digit(CI_lower, digits),
              ", ", format_digit(CI_higher, digits), "]")')
  } else{
    edges <- edges %>%
      mutate_('Label' = 'format_digit(Coef, digits)')
  }
  edges <- edges %>%
    mutate_('Label_Regression' = "ifelse(Type=='Regression', Label, '')",
            'Label_Correlation' = "ifelse(Type=='Correlation', Label, '')",
            'Label_Loading' = "ifelse(Type=='Loading', Label, '')")
  edges <- edges[colSums(!is.na(edges)) > 0]

  # Identify latent variables for nodes
  latent_nodes <- edges %>%
    filter_('Type == "Loading"') %>%
    distinct_("to") %>%
    transmute_("Name" = "to", "Latent" = TRUE)

  nodes_list <- unique(c(edges$from, edges$to))

  # Node properties
  nodes <- summary %>%
    filter_("From == To",
            "From %in% nodes_list") %>%
    mutate_("Name" = "From") %>%
    left_join(latent_nodes, by="Name") %>%
    mutate_("Latent" = "if_else(is.na(Latent), FALSE, Latent)") %>%
    select(one_of(c("Name", "Latent")))

  return(list(nodes = nodes, edges = edges))
}





#' Get graph data from factor analysis.
#'
#' Get graph data from fa objects.
#'
#' @param fit psych::fa object.
#' @param threshold_Coef Omit all links with a Coefs below this value.
#' @param digits Edges' labels rounding.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A list containing nodes and edges data to be used by `tidygraph::tbl_graph()`.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
get_graph.fa <- function(fit, threshold_Coef=NULL, digits=2, ...){
  edges <- summary(analyze(fit)) %>%
    gather("To", "Coef", -one_of("N", "Item", "Label")) %>%
    rename_("From" = "Item") %>%
    mutate_("Label" = "format_digit(Coef, digits)") %>%
    select(one_of("From", "To", "Coef", "Label"), everything()) %>%
    filter()

  # Deal with thresholds
  if(is.null(threshold_Coef)){
    threshold_Coef <- min(abs(edges$Coef))-1
  }

  edges <- edges %>%
    filter_("Coef > threshold_Coef")

  nodes <- data.frame("Name" = c(edges$From, edges$To)) %>%
    distinct_("Name")

  return(list(nodes = nodes, edges = edges))

}




#' Get graph data from correlation.
#'
#' Get graph data from correlation.
#'
#' @param fit Object from psycho::correlation.
#' @param ... Arguments passed to or from other methods.
#'
#' @return A list containing nodes and edges data to be used by `igraph::graph_from_data_frame()`.
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#'
#' @export
get_graph.psychobject_correlation <- function(fit, ...){

  vars <- row.names(fit$values$r)

  r <- fit$values$r %>%
    as.data.frame() %>%
    tibble::rownames_to_column("from") %>%
    tidyr::gather("to", "r", vars)

  if("p" %in% names(fit$values)){
    r <- r %>%
      full_join(
        fit$values$p %>%
          as.data.frame() %>%
          tibble::rownames_to_column("from") %>%
          tidyr::gather("to", "p", vars), by = c("from", "to"))
  }

  r <- filter(r, !from == to)
  return(r)
}
