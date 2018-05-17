#' Dprime and Other Signal Detection Theory indices.
#'
#' Computes Signal Detection Theory indices (d', beta, A', B''D, c).
#'
#' @param n_hit Number of hits.
#' @param n_fa Number of false alarms.
#' @param n_miss Number of misses.
#' @param n_cr Number of correct rejections.
#' @param n_targets Number of targets (n_hit + n_miss).
#' @param n_distractors Number of distractors (n_fa + n_cr).
#' @param adjusted Should it use the Hautus (1995) adjustments for extreme values.
#'
#' @return Calculates the d', the beta, the A' and the B''D based on the signal detection theory (SRT). See Pallier (2002) for the algorithms.
#'
#' Returns a list containing 4 objects:
#' \itemize{
#'  \item{\strong{dprime (d')}: }{The sensitivity. Reflects the distance between the two distributions: signal, and signal+noise and corresponds to the Z value of the hit-rate minus that of the false-alarm rate.}
#'  \item{\strong{beta}: }{The bias (criterion). The value for beta is the ratio of the normal density functions at the criterion of the Z values used in the computation of d'. This reflects an observer's bias to say 'yes' or 'no' with the unbiased observer having a value around 1.0. As the bias to say 'yes' increases (liberal), resulting in a higher hit-rate and false-alarm-rate, beta approaches 0.0. As the bias to say 'no' increases (conservative), resulting in a lower hit-rate and false-alarm rate, beta increases over 1.0 on an open-ended scale.}
#'  \item{\strong{aprime (A')}: }{Non-parametric estimate of discriminability. An A' near 1.0 indicates good discriminability, while a value near 0.5 means chance performance.}
#'  \item{\strong{bppd (B''D)}: }{Non-parametric estimate of bias. A B''D equal to 0.0 indicates no bias, positive numbers represent conservative bias (i.e., a tendency to answer 'no'), negative numbers represent liberal bias (i.e. a tendency to answer 'yes'). The maximum absolute value is 1.0.}
#'  \item{\strong{c}: }{Another index of bias. the number of standard deviations from the midpoint between these two distributions, i.e., a measure on a continuum from "conservative" to "liberal".}
#'  }
#'
#'
#' Note that for d' and beta, adjustement for extreme values are made following the recommandations of Hautus (1995).


#' @examples
#' library(psycho)
#'
#' n_hit <- 9
#' n_fa <- 2
#' n_miss <- 1
#' n_cr <- 7
#'
#' indices <- psycho::dprime(n_hit, n_fa, n_miss, n_cr)
#'
#'
#' df <- data.frame(Participant = c("A", "B", "C"),
#'     n_hit = c(1, 2, 5),
#'     n_fa = c(6, 8, 1))
#'
#' indices <- psycho::dprime(n_hit=df$n_hit,
#'     n_fa=df$n_fa,
#'     n_targets=10,
#'     n_distractors=10,
#'     adjusted=FALSE)
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats qnorm
#' @export
dprime <- function(n_hit, n_fa, n_miss=NULL, n_cr=NULL, n_targets=NULL, n_distractors=NULL, adjusted=TRUE) {
  if (is.null(n_targets)) {
    n_targets <- n_hit + n_miss
  }

  if (is.null(n_distractors)) {
    n_distractors <- n_fa + n_cr
  }


  # Parametric Indices ------------------------------------------------------


  if (adjusted == TRUE) {
    if (is.null(n_miss) | is.null(n_cr)) {
      warning("Please provide n_miss and n_cr in order to compute adjusted ratios. Computing indices anyway with non-adjusted ratios...")

      # Non-Adjusted ratios
      hit_rate_adjusted <- n_hit / n_targets
      fa_rate_adjusted <- n_fa / n_distractors
    } else {
      # Adjusted ratios
      hit_rate_adjusted <- (n_hit + 0.5) / ((n_hit + 0.5) + n_miss + 1)
      fa_rate_adjusted <- (n_fa + 0.5) / ((n_fa + 0.5) + n_cr + 1)
    }

    # dprime
    dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)

    # beta
    zhr <- qnorm(hit_rate_adjusted)
    zfar <- qnorm(fa_rate_adjusted)
    beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)

    # c
    c <- -(qnorm(hit_rate_adjusted) + qnorm(fa_rate_adjusted)) / 2
  } else {
    # Ratios
    hit_rate <- n_hit / n_targets
    fa_rate <- n_fa / n_distractors

    # dprime
    dprime <- qnorm(hit_rate) - qnorm(fa_rate)

    # beta
    zhr <- qnorm(hit_rate)
    zfar <- qnorm(fa_rate)
    beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)

    # c
    c <- -(qnorm(hit_rate) + qnorm(fa_rate)) / 2
  }

  # Non-Parametric Indices ------------------------------------------------------

  # Ratios
  hit_rate <- n_hit / n_targets
  fa_rate <- n_fa / n_distractors

  # aprime
  a <- 1 / 2 + ((hit_rate - fa_rate) * (1 + hit_rate - fa_rate) / (4 * hit_rate * (1 - fa_rate)))
  b <- 1 / 2 - ((fa_rate - hit_rate) * (1 + fa_rate - hit_rate) / (4 * fa_rate * (1 - hit_rate)))

  a[fa_rate > hit_rate] <- b[fa_rate > hit_rate]
  a[fa_rate == hit_rate] <- .5
  aprime <- a

  # bppd
  bppd <- ((1 - hit_rate) * (1 - fa_rate) - hit_rate * fa_rate) / ((1 - hit_rate) * (1 - fa_rate) + hit_rate * fa_rate)


  return(list(dprime = dprime, beta = beta, aprime = aprime, bppd = bppd, c = c))
}
