#' Mellenbergh & van den Brink (1998) test for pre-post comparison.
#'
#' Test for comparing post-test to baseline for a single participant.
#'
#' @param t0 Single value (pretest or baseline score).
#' @param t1 Single value (posttest score).
#' @param controls Vector of scores of the control group OR single value corresponding to the control SD of the score.
#' @param verbose True or False. Prints the interpretation text.
#'
#' @return Returns a data frame containing the z-value and p-value. If significant, the difference between pre and post tests is significant.
#'
#' @examples
#' mellenbergh.test(t0 = 4, t1 = 12, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#'
#' @author Dominique Makowski
#'
#' @importFrom stats pnorm sd
#' @export
mellenbergh.test <- function(t0, t1, controls, verbose=T){

  if (length(controls) > 1){
    sd <- sd(controls) * sqrt(2)
  } else {
    sd <- controls * sqrt(2)
  }

  diff <- t1 - t0

  diff_CI_bottom <- diff - 1.65 * sd
  diff_CI_top <- diff + 1.65 * sd

  z <- diff / sd
  pval <- 2*pnorm(-abs(z))


  p_interpretation <- ifelse(pval < 0.05, " ", " not ")
  text <- paste0("The Mellenbergh & van den Brink (1998) test suggests that the change is",
                 p_interpretation,
                 "significant (d = ",
                 format_digit(diff),
                 ", 90% CI [",
                 format_digit(diff_CI_bottom),
                 ", ",
                 format_digit(diff_CI_top),
                 "], z = ",
                 format_digit(z),
                 ", p ",
                 format_p(pval),
                 ").")

  result <- data.frame(diff = diff, diff_90_CI_lower = diff_CI_bottom, diff_90_CI_higher = diff_CI_top, z = z, p=pval)

  if (verbose == T){
    cat(paste0(text, "\n\n"))
  }

  return(result)
  #   return("The method for no-controls is not implemented yet.")
}