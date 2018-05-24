#' Performs a Bayesian correlation.
#'
#' Performs a Bayesian correlation.
#'
#' @param x First continuous variable.
#' @param y Second continuous variable.
#' @param CI Credible interval bounds.
#' @param iterations The number of iterations to sample.
#'
#' @return A psychobject.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' x <- psycho::affective$Concealing
#' y <- psycho::affective$Tolerating
#'
#' bayesian_cor.test(x, y)
#' summary(bayesian_cor.test(x, y))
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom BayesFactor correlationBF posterior
#' @importFrom stats complete.cases cor.test
#' @import dplyr
#' @export
bayesian_cor.test <- function(x, y, CI=90, iterations = 10000) {


  # Varnames ----------------------------------------------------------------


  if (is.null(names(x))) {
    var1 <- deparse(substitute(x))
  } else {
    var1 <- names(x)
    x <- pull(x)
  }

  if (is.null(names(y))) {
    var2 <- deparse(substitute(y))
  } else {
    var2 <- names(y)
    y <- pull(y)
  }

  # Remove missing
  var_x <- x[complete.cases(x, y)]
  var_y <- y[complete.cases(x, y)]

  # Correlation -------------------------------------------------------------


  if (cor.test(var_x, var_y)$estimate > 0.999) {
    return(1)
  }
  cor <- BayesFactor::correlationBF(var_x, var_y)
  posterior <- as.vector(suppressMessages(BayesFactor::posterior(cor, iterations = iterations, progress = FALSE)))

  values <- list()
  values$posterior <- posterior
  values$bf <- as.vector(cor)[1]
  values$median <- median(posterior)
  values$mad <- mad(posterior)
  values$mean <- mean(posterior)
  values$sd <- sd(posterior)
  values$CI <- hdi(posterior, prob = CI / 100)$text
  values$CI_values <- hdi(posterior, prob = CI / 100)
  values$CI_values <- c(values$CI_values$values$HDImin, values$CI_values$values$HDImax)
  values$MPE <- mpe(posterior)$MPE
  values$MPE_values <- mpe(posterior)$values

  norm <- rnorm_perfect(length(posterior), 0, sd(posterior))
  values$overlap <- overlap(posterior, norm) * 100

  rope_indices <- rope(posterior, bounds = c(-0.1, 0.1), CI = 95, overlap = TRUE)
  values$rope_decision <- rope_indices$rope_decision
  values$rope_probability <- rope_indices$rope_probability
  values$rope_overlap <- rope_indices$rope_overlap


  summary <- data.frame(
    Median = values$median,
    MAD = values$mad,
    Mean = values$mean,
    SD = values$sd,
    CI_lower = values$CI_values[1],
    CI_higher = values$CI_values[2],
    MPE = values$MPE,
    BF = values$bf,
    Overlap = values$overlap,
    Rope = values$rope_decision
  )
  rownames(summary) <- paste0(var1, " / ", var2)

  values$effect_size <- interpret_r_posterior(posterior)
  interpretation_r <- interpret_r(values$median, strength = FALSE)
  interpretation_bf <- interpret_bf(values$bf)

  text <- paste0(
    "Results of the Bayesian correlation indicate ",
    interpretation_bf,
    " a ",
    interpretation_r,
    " association between ",
    var1,
    " and ",
    var2,
    " (r = ",
    format_digit(values$median),
    ", MAD = ",
    format_digit(values$mad),
    ", ",
    CI,
    "% CI [",
    format_digit(values$CI_values[1], null_treshold = 0.0001),
    ", ",
    format_digit(values$CI_values[2], null_treshold = 0.0001),
    "], MPE = ",
    format_digit(values$MPE),
    "%, BF = ",
    format_digit(values$bf),
    "). ",
    values$effect_size$text
  )

  plot <- "Not available."

  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")

  return(output)
}





#' Bayesian Correlation Matrix.
#'
#' Bayesian Correlation Matrix.
#'
#' @param df The dataframe.
#'
#' @return A list of dataframes
#'
#' @examples
#' library(psycho)
#' df <- psycho::affective
#' bayesian_cor(df)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @export
bayesian_cor <- function(df) {
  df <- purrr::keep(df, is.numeric)

  r <- matrix(0, nrow = ncol(df), ncol = ncol(df))
  mpe <- matrix(0, nrow = ncol(df), ncol = ncol(df))
  bf <- matrix(0, nrow = ncol(df), ncol = ncol(df))
  ci <- matrix(0, nrow = ncol(df), ncol = ncol(df))

  for (j in 1:ncol(df)) {
    for (i in 1:ncol(df)) {
      x <- df[, i]
      y <- df[, j]
      result <- bayesian_cor.test(x, y)
      if (!is.psychobject(result)) {
        r[i, j] <- 1
        mpe[i, j] <- 100
        bf[i, j] <- Inf
        ci[i, j] <- "100% CI [1, 1]"
      } else {
        r[i, j] <- result$values$median
        mpe[i, j] <- result$values$MPE
        bf[i, j] <- result$values$bf
        ci[i, j] <- result$values$CI
      }
    }
  }

  output <- list(
    r = r,
    mpe = mpe,
    bf = bf,
    ci = ci
  )
  return(output)
}
