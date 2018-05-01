#' Find season of dates.
#'
#' Returns the season of an array of dates.
#'
#' @param date Array of dates. Must cover the 4 seasons.
#'
#' @return season
#'
#' @examples
#' library(psycho)
#'
#' dates <- c("2017-02-15", "2017-05-15", "2017-08-15", "2017-11-15")
#' find_season(dates)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_season <- function(date) {
  d <- as.Date(cut(as.Date(date), "month")) + 32
  season <- factor(
    quarters(d),
    labels = c("Winter", "Spring", "Summer", "Fall")
  )
  return(season)
}
