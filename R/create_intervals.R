#' Overlap of Two Empirical Distributions.
#'
#' A method to calculate the overlap coefficient of two kernel density estimates (a measure of similarity between two samples).
#'
#' @param x A vector of numerics.
#' @param n Number of intervals to create, OR
#' @param length Length of each interval.
#' @param equal_range Makes n groups with with equal range (TRUE) or (approximately) equal numbers of observations (FALSE).
#' @param labels Can be a custom list, "NULL", "FALSE" or "median".
#' @param dig.lab Integer which is used when labels are not given. It determines the number of digits used in formatting the break numbers.
#'
#' @examples
#' library(psycho)
#' 
#' x <- rnorm(100, 0, 1)
#' 
#' create_intervals(x, n = 4)
#' create_intervals(x, n = 4, equal_range = FALSE)
#' create_intervals(x, length = 1)
#' 
#' create_intervals(x, n = 4, labels = "median")
#' create_intervals(x, n = 4, labels = FALSE)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom ggplot2 cut_interval cut_number
#' @export
create_intervals <- function(x, n = NULL, length = NULL, equal_range = TRUE, labels = NULL, dig.lab = 3) {
  if (equal_range) {
    if (is.character(labels) && labels == "median") {
      cuts <- ggplot2::cut_interval(x, n = n, length = length, labels = FALSE)
    } else {
      cuts <- ggplot2::cut_interval(x, n = n, length = length, labels = labels, dig.lab = dig.lab)
    }
  } else {
    if (is.character(labels) && labels == "median") {
      cuts <- ggplot2::cut_number(x, n = n, labels = FALSE)
    } else {
      cuts <- ggplot2::cut_number(x, n = n, labels = labels, dig.lab = dig.lab)
    }
  }


  if (is.character(labels) && labels == "median") {
    cuts <- cuts %>%
      data.frame(x) %>%
      group_by_(".") %>%
      mutate_("cuts" = "median(x)") %>%
      ungroup() %>%
      select_("cuts") %>%
      pull()
  }

  return(cuts)
}
