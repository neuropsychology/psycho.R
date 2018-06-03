#' Bayes Factor Interpretation
#'
#' Return the interpretation of a Bayes Factor.
#'
#' @param x Bayes Factor.
#' @param direction Include direction (against / in favour).
#' @param bf Include Bayes Factor.
#'
#' @return The interpretation according to Jeffreys (1961).
#'
#' @examples
#' library(psycho)
#' interpret_bf(10)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @references
#' \itemize{
#'  \item{Jeffreys, H. (1961), Theory of Probability, 3rd ed., Oxford University Press, Oxford.}
#'  \item{Jarosz, A. F., & Wiley, J. (2014). What are the odds? A practical guide to computing and reporting Bayes factors. The Journal of Problem Solving, 7(1), 2.}
#'  }
#' @export
interpret_bf <- function(x, direction=TRUE, bf=TRUE) {
  if (x < 1) {
    x <- 1 / abs(x)
    dir <- "against"
  } else {
    dir <- "in favour of"
  }



  interpretation <- ifelse(abs(x) > 100, "extreme evidence",
    ifelse(abs(x) >= 30, "very strong evidence",
      ifelse(abs(x) >= 10, "strong evidence",
        ifelse(abs(x) >= 3, "moderate evidence",
          ifelse(abs(x) >= 1, "anecdotal evidence", "no evidence")
        )
      )
    )
  )

  if(bf == TRUE){
    bf <- paste0("(", format_bf(x), ")")
    interpretation <- paste(interpretation, bf)
  }
  if (direction == TRUE) {
    interpretation <- paste(interpretation, dir)
  }

  return(interpretation)
}





#' Bayes factor formatting
#'
#' Bayes factor formatting
#'
#' @param bf Bayes Factor.
#' @param max Treshold for maximum.
#'
#' @export
format_bf <- function(bf, max=100){

  if(bf > max){
    bf <- paste0("BF > ", max)
  } else{
    bf <- paste0("BF = ", format_digit(bf))
  }
  return(bf)
}

