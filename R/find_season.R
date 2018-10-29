#' Find season of dates.
#'
#' Returns the season of an array of dates.
#'
#' @param dates Array of dates.
#' @param winter month-day of winter solstice.
#' @param spring month-day of spring equinox.
#' @param summer month-day of summer solstice.
#' @param fall month-day of fall equinox.
#'
#' @return season
#'
#' @examples
#' library(psycho)
#'
#' dates <- c("2012-02-15", "2017-05-15", "2009-08-15", "1912-11-15")
#' find_season(dates)
#'
#' @author Josh O'Brien
#'
#' @seealso
#' https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
#'
#' @export
find_season <- function(dates, winter = "12-21", spring = "3-20", summer = "6-21", fall = "9-22") {
  WS <- as.Date(paste0("2012-", winter), format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date(paste0("2012-", spring), format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date(paste0("2012-", summer), format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date(paste0("2012-", fall), format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(as.character(dates), format = "2012-%m-%d"))

  season <- ifelse(d >= WS | d < SE, "Winter",
    ifelse(d >= SE & d < SS, "Spring",
      ifelse(d >= SS & d < FE, "Summer", "Fall")
    )
  )
  return(season)
}
