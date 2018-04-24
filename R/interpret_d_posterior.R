#' Return the effect sizes for the posterior of a standardized coefficient following Cohen (1988).
#'
#' @param posterior Posterior distribution of standardized coefficient.
#'
#' @return The interpretation according to Cohen (1988)
#'
#' @examples
#' library(psycho)
#' interpret_d_posterior(rnorm(1000, 0, 1))
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
interpret_d_posterior <- function(posterior) {


  # Compute the probabilities
  mkneg <- function(pmin, pmax) {
    stopifnot(pmin < pmax) # sanity check
    length(posterior[posterior > pmin & posterior <= pmax]) /
      length(posterior)
  }

  mkpos <- function(pmin, pmax) {
    stopifnot(pmin < pmax) # sanity check
    length(posterior[posterior >= pmin & posterior < pmax]) /
      length(posterior)
  }

  verylarge_neg <- mkneg(-Inf, -1.3)
  large_neg <- mkneg(-1.3, -0.8)
  medium_neg <- mkneg(-0.8, -0.5)
  small_neg <- mkneg(-0.5, -0.2)
  verysmall_neg <- mkneg(-0.2, 0) # TODO: there was open interval at 0

  verylarge_pos <- mkpos(1.3, Inf)
  large_pos <- mkpos(0.8, 1.3)
  medium_pos <- mkpos(0.5, 0.8)
  small_pos <- mkpos(0.2, 0.5)
  verysmall_pos <- mkpos(0, 0.2) # TODO: there was open interval at 0

  EffSize <- data.frame(
    Direction = c(
      "Negative", "Negative", "Negative", "Negative",
      "Negative", "Positive", "Positive", "Positive",
      "Positive", "Positive"
    ),
    Size = c(
      "VeryLarge", "Large", "Medium", "Small", "VerySmall",
      "VerySmall", "Small", "Medium", "Large", "VeryLarge"
    ),
    Probability = c(
      verylarge_neg, large_neg, medium_neg, small_neg,
      verysmall_neg, verysmall_pos, small_pos, medium_pos,
      large_pos, verylarge_pos
    )
  )

  EffSize$Probability[is.na(EffSize$Probability)] <- 0

  if (mean(posterior) >= 0) {
    opposite_prob <-
      sum(EffSize$Probability[EffSize$Direction == "Negative"])
    if (length(posterior[posterior > 0]) > 0) {
      opposite_max <- min(posterior[posterior > 0])
    } else {
      opposite_max <- 0
    }
    verylarge <- verylarge_pos
    large <- large_pos
    medium <- medium_pos
    small <- small_pos
    verysmall <- verysmall_pos
  } else {
    opposite_prob <-
      sum(EffSize$Probability[EffSize$Direction == "Positive"])
    if (length(posterior[posterior > 0]) > 0) {
      opposite_max <- max(posterior[posterior > 0])
    } else {
      opposite_max <- 0
    }
    verylarge <- verylarge_neg
    large <- large_neg
    medium <- medium_neg
    small <- small_neg
    verysmall <- verysmall_neg
  }

  EffSize_text <- paste0(
    "    - There is a probability of ",
    format_digit(verylarge * 100),
    "% that this effect size is very large, ",
    format_digit(large * 100),
    "% that this effect size is large, ",
    format_digit(medium * 100),
    "% that this effect size is medium, ",
    format_digit(small * 100),
    "% that this effect size is small, ",
    format_digit(verysmall * 100),
    "% that this effect is very small and ",
    format_digit(opposite_prob * 100),
    "% that it has an opposite direction",
    " (between 0 and ", format_digit(opposite_prob), ")."
  )


  interpretation <- list(
    table = EffSize,
    text = EffSize_text,
    probs = list(
      VeryLarge = verylarge,
      Large = large,
      Medium = medium,
      Small = small,
      VerySmall = verysmall,
      Opposite = opposite_prob
    )
  )

  return(interpretation)
}
