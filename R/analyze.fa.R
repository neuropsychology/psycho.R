#' Analyze fa objects.
#'
#' Analyze fa objects.
#'
#' @param x An psych object.
#' @param labels Supply a additional column with e.g. item labels.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(psych)
#'
#' x <- psych::fa(psych::Thurstone.33, 2)
#'
#' results <- analyze(x)
#' print(results)
#' summary(results)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
analyze.fa <- function(x, labels=NULL, ...) {
  loadings <- format_loadings(x, labels)

  values <- list()
  values$loadings <- loadings$loadings
  values$loadings_max <- loadings$max
  values$cfa_model <- get_cfa_model(loadings$max)

  text <- format(values$cfa_model)
  summary <- values$loadings
  plot <- plot_loadings(values$loadings)

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}







#' Format the loadings of a factor analysis.
#'
#' Format the loadings of a factor analysis.
#'
#' @param x An psych object.
#' @param labels Supply a additional column with e.g. item labels.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' x <- psych::fa(psych::Thurstone.33, 2)
#' format_loadings(x)
#' }
#'
#' @import dplyr
#' @export
format_loadings <- function(x, labels=NULL) {


  # Check loadings and remove those inferior to a treshold
  loadings <- x$loadings %>%
    unclass() %>%
    as.data.frame()

  # Save n factors
  n_factors <- length(loadings)

  # Add item labels
  loadings$Item <- rownames(loadings)
  if (length(labels) == nrow(loadings)) {
    loadings$Label <- labels
  } else {
    loadings$Label <- 1:nrow(loadings)
  }

  # Keep Order
  loadings$N <- 1:nrow(loadings)


  # Select the max loading for each item
  max <- get_loadings_max(loadings)


  # Reorder the loading matrix accordingly
  loadings <- loadings[max$N, ] %>%
    select_("N", "Item", "Label", "everything()")

  return(list(loadings = loadings, max = max))
}



#' Get loadings max.
#'
#' Get loadings max.
#'
#' @param loadings Formatted loadings.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' x <- psych::fa(psych::Thurstone.33, 2)
#' get_loadings_max(format_loadings(x)$loadings)
#' }
#'
#'
#' @import dplyr
#' @export
get_loadings_max <- function(loadings) {
  max <- loadings %>%
    tidyr::gather_("Component", "Loading", names(loadings)[!names(loadings) %in% c("Item", "N", "Label")]) %>%
    dplyr::group_by_("Item") %>%
    dplyr::slice_("which.max(abs(Loading))") %>%
    dplyr::arrange_("Component", "desc(Loading)")
  return(max)
}



#' Get CFA model.
#'
#' Get CFA model.
#'
#' @param loadings_max Max loadings by variable.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' x <- psych::fa(psych::Thurstone.33, 2)
#' get_cfa_model(format_loadings(x)$max)
#' }
#'
#'
#' @import dplyr
#' @export
get_cfa_model <- function(loadings_max) {
  cfa_model <- loadings_max %>%
    select_("Item", "Component") %>%
    group_by_("Component") %>%
    summarise_("Observed" = 'paste(Item, collapse=" + ")') %>%
    transmute_("Latent_Variable" = 'paste(Component, Observed, sep=" =~ ")') %>%
    pull()

  cfa_model <- c("#Latent variables", cfa_model) %>%
    paste(collapse = "\n")

  return(cfa_model)
}




#' Plot loadings.
#'
#' Plot loadings.
#'
#' @param loadings Loadings by variable.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' x <- psych::fa(psych::Thurstone.33, 2)
#' plot_loadings(format_loadings(x)$loadings)
#' }
#'
#'
#' @import dplyr
#' @export
plot_loadings <- function(loadings) {
  p <- loadings %>%
    gather("Component", "Loading", matches("\\d$")) %>%
    mutate_("Loading" = "abs(Loading)") %>%
    mutate_("Item" = "factor(Item, levels=get_loadings_max(loadings)$Item)") %>%
    ggplot(aes_string(y = "Loading", x = "Item", fill = "Component")) +
    geom_bar(stat = "identity") +
    coord_flip() +
    ylab("\nLoading Strength") +
    xlab("Item\n")

  return(p)
}
