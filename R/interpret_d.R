#' Return the interpretation of a Cohen's d or a standardized coefficient following Cohen (1988).
#'
#' @param x Cohen's d value of standardized coefficient.
#'
#' @return The interpretation according to Cohen (1988)
#'
#' @examples
#' library(psycho)
#' interpret_d(-0.42)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d <- function(x){

  interpretation <- ifelse(abs(x) > 1.3, "Very Large",
                           ifelse(abs(x) >= 0.8, "Large",
                           ifelse(abs(x) >= 0.5, "Medium",
                           ifelse(abs(x) >= 0.2, "Small", "Very Small"))))

  return(interpretation)
}
