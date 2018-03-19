#' Dprime and Other Signal Detection Theory indices.
#'
#' Computes Signal Detection Theory indices (d', beta, A', B''D, c).
#'
#' @param n_hit Number of hits.
#' @param n_miss Number of misses.
#' @param n_fa Number of false alarms.
#' @param n_cr Number of correct rejections.
#'
#' @return Calculates the d', the beta, the A' and the B''D based on the signal detection theory (SRT). See Pallier (2002) for the algorithms.
#'
#' Returns a list containing 4 objects:
#' \itemize{
#'  \item{dprime (d'): }{The sensitivity. Reflects the distance between the two distributions: signal, and signal+noise and corresponds to the Z value of the hit-rate minus that of the false-alarm rate.}
#'  \item{beta: }{The bias (criterion). The value for beta is the ratio of the normal density functions at the criterion of the Z values used in the computation of d'. This reflects an observer's bias to say 'yes' or 'no' with the unbiased observer having a value around 1.0. As the bias to say 'yes' increases (liberal), resulting in a higher hit-rate and false-alarm-rate, beta approaches 0.0. As the bias to say 'no' increases (conservative), resulting in a lower hit-rate and false-alarm rate, beta increases over 1.0 on an open-ended scale.}
#'  \item{aprime (A'): }{Non-parametric estimate of discriminability. An A' near 1.0 indicates good discriminability, while a value near 0.5 means chance performance.}
#'  \item{bppd (B''D): }{Non-parametric estimate of bias. A B''D equal to 0.0 indicates no bias, positive numbers represent conservative bias (i.e., a tendency to answer 'no'), negative numbers represent liberal bias (i.e. a tendency to answer 'yes'). The maximum absolute value is 1.0.}
#'  \item{c: }{Another index of bias. the number of standard deviations from the midpoint between these two distributions, i.e., a measure on a continuum from "conservative" to "liberal".}
#' }
#'
#' Note that for d' and beta, adjustement for extreme values are made following the recommandations Hautus (1995).


#' @examples
#' n_hit <- 9
#' n_miss <- 1
#' n_fa <- 2
#' n_cr <- 7
#'
#' indices <- dprime(n_hit, n_miss, n_fa, n_cr)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats qnorm
#' @export
dprime <- function(n_hit, n_miss, n_fa, n_cr) {
  # Ratios
  hit_rate <- n_hit / (n_hit + n_miss)
  fa_rate <- n_fa / (n_fa + n_cr)


  # Adjusted ratios
  hit_rate_adjusted <- (n_hit + 0.5) / ((n_hit + 0.5) + n_miss + 1)
  fa_rate_adjusted <- (n_fa + 0.5) / ((n_fa + 0.5) + n_cr + 1)


  # dprime
  dprime <- qnorm(hit_rate_adjusted) - qnorm(fa_rate_adjusted)

  # beta
  zhr <- qnorm(hit_rate_adjusted)
  zfar <- qnorm(fa_rate_adjusted)
  beta <- exp(-zhr * zhr / 2 + zfar * zfar / 2)

  # aprime
  a <- 1 / 2 + ((hit_rate - fa_rate) * (1 + hit_rate - fa_rate) / (4 * hit_rate * (1 - fa_rate)))
  b <- 1 / 2 - ((fa_rate - hit_rate) * (1 + fa_rate - hit_rate) / (4 * fa_rate * (1 - hit_rate)))

  a[fa_rate > hit_rate] <- b[fa_rate > hit_rate]
  a[fa_rate == hit_rate] <- .5
  aprime <- a

  # bppd
  bppd <- ((1 - hit_rate) * (1 - fa_rate) - hit_rate * fa_rate) / ((1 - hit_rate) * (1 - fa_rate) + hit_rate * fa_rate)

  # c
  c <- -(qnorm(hit_rate_adjusted) + qnorm(fa_rate_adjusted)) / 2

  return(list(dprime = dprime, beta = beta, aprime = aprime, bppd = bppd, c = c))
}
