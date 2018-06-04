#' Bayes Factor Interpretation
#'
#' Return the interpretation of a Bayes Factor.
#'
#' @param x Bayes Factor.
#' @param direction Include direction (against / in favour).
#' @param bf Include Bayes Factor.
#' @param rules Can be "jeffreys1961" (default), "raftery1995", or a custom list.
#'
#' @return The interpretation according to Jeffreys (1961).
#'
#' @examples
#' library(psycho)
#' interpret_bf(x=10)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @references
#' \itemize{
#'  \item{Jeffreys, H. (1961), Theory of Probability, 3rd ed., Oxford University Press, Oxford.}
#'  \item{Jarosz, A. F., & Wiley, J. (2014). What are the odds? A practical guide to computing and reporting Bayes factors. The Journal of Problem Solving, 7(1), 2.}
#'  }
#' @export
interpret_bf <- function(x, direction=TRUE, bf=TRUE, rules="jeffreys1961") {
  interpretation <- sapply(x, .interpret_bf, direction = direction, bf = bf, rules = rules, return_rules = FALSE)
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
format_bf <- function(bf, max=100) {
  if (bf > max) {
    bf <- paste0("BF > ", max)
  } else {
    bf <- paste0("BF = ", format_digit(bf))
  }
  return(bf)
}










#' @keywords internal
.interpret_bf <- function(x, direction=TRUE, bf=TRUE, rules="jeffreys1961", return_rules=TRUE) {
  if (x < 1) {
    x <- 1 / abs(x)
    dir <- "against"
  } else {
    dir <- "in favour of"
  }


  if (!is.list(rules)) {
    if (rules == "jeffreys1961") {
      rules <- list(
        "no" = 0,
        "anecdotal" = 1,
        "moderate" = 3,
        "strong" = 10,
        "very strong" = 30,
        "extreme" = 100
      )
    } else if (rules == "raftery1995") {
      rules <- list(
        "no" = 0,
        "weak" = 1,
        "positive" = 3,
        "strong" = 20,
        "very strong" = 150
      )
    } else {
      stop("rules must be either a list or 'jeffreys1961' or 'raftery1995'.")
    }
  }



  s <- (abs(x) - unlist(rules))
  s <- names(which.min(s[s >= 0]))
  if (is.null(s)) {
    s <- NA
  } else {
    s <- paste(s, "evidence")
  }




  if (bf == TRUE) {
    bf <- paste0("(", format_bf(x), ")")
    s <- paste(s, bf)
  }
  if (direction == TRUE) {
    interpretation <- paste(s, dir)
  }

  return(interpretation)
}
