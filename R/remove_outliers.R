#' Remove outliers.
#'
#' Removes outliers (with the z-score method only for now).
#'
#' @param df Dataframe.
#' @param target String or list of strings of variables
#' @param threshold The z-score value (deviation of SD) by which to consider outliers.
#' @param direction Can be "both", "upper" or "lower".
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
remove_outliers <- function(df, target, threshold = qnorm(0.95), direction = "both") {
  for (var in c(target)) {
    df <- .remove_outliers(df, var, threshold, direction)
  }
  return(df)
}






#' @keywords internal
.remove_outliers <- function(df, target, threshold = qnorm(0.95), direction = "both") {
  df <- df %>%
    mutate_("outlier_criterion" = target) %>%
    standardize(subset = "outlier_criterion")
  if (direction %in% c("both", "upper")) {
    df <- df %>%
      filter_("outlier_criterion <= threshold")
  }
  if (direction %in% c("both", "lower")) {
    df <- df %>%
      filter_("outlier_criterion >= -threshold")
  }

  df <- df %>%
    select_("-outlier_criterion")

  return(df)
}
