#' Return the interpretation of a Bayes Factor.
#'
#' @param x Bayes Factor.
#' @param label_only Keep only the size classification.
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
interpret_bf <- function(x, label_only=FALSE) {
  if (x < 1) {
    x <- 1 / abs(x)
    direction <- "against"
  } else {
    direction <- "in favour of"
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

  if (label_only == FALSE) {
    interpretation <- paste(interpretation, direction)
  }
  return(interpretation)
}
