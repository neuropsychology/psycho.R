#' Analyze fa objects.
#'
#' Analyze fa objects.
#'
#' @param x An psych object.
#' @param labels Supply a additional column with e.g. item labels.
#' @param treshold 'max' or numeric. The treshold over which to associate an item with its component.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(psych)
#'
#' x <- psych::pca(psych::Thurstone.33, 2)
#'
#' results <- analyze(x)
#' print(results)
#' summary(results)
#' plot(results)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
analyze.principal <- function(x, labels = NULL, treshold = "max", ...) {
  loadings <- format_loadings(x, labels)

  values <- list()
  values$variance <- x$Vaccounted
  values$loadings <- loadings$loadings
  values$loadings_max <- loadings$max
  values$cfa_model <- get_cfa_model(loadings$loadings, treshold = treshold)

  text <- .fa_variance_text(values$variance)
  text <- paste0(text, "\n\n", format(values$cfa_model))
  summary <- values$loadings
  plot <- plot_loadings(values$loadings)

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
