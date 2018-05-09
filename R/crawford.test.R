#'  Crawford-Garthwaite (2007) Bayesian test for single-case analysis.
#'
#' Neuropsychologists often need to compare a single case to a small control group. However, the standard two-sample t-test does not work because the case is only one observation. Crawford and Garthwaite (2012) demonstrate that the Crawford-Garthwaite (2007) Bayesian test is a better approach than other commonly-used alternatives.
#' .
#'
#' @param patient Single value (patient's score).
#' @param controls Vector of values (control's scores).
#' @param mean Mean of the control sample.
#' @param sd SD of the control sample.
#' @param n Size of the control sample.
#' @param CI Credible interval bounds.
#' @param iter Number of iterations.
#' @param color Main color of plot distribution.
#' @param color_CI Color of CI area.
#' @param color_score Color of the line representing the patient's score.
#' @param color_size Size of the line representing the patient's score.
#'
#'
#' @details The p value obtained when this test is used to test significance also simultaneously provides a point estimate of the abnormality of the patient’s score; for example if the one-tailed probability is .013 then we know that the patient’s score is significantly (p , .05) below the control mean and that it is estimated that 1.3% of the control population would obtain a score lower than the patient’s. As for the credible interval interpretation, we could say that there is a 95% probability that the true level of abnormality of the patient’s score lies within the stated limits, or that There is 95% confidence that the percentage of people who have a score lower than the patient’s is between 0.01% and 6.66%.
#'
#' @examples
#' library(psycho)
#'
#' crawford.test(patient = 10, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#' test <- crawford.test(patient = 7, controls = c(0, -2, 5, -6, 0, 3, -4, -2))
#' plot(test)
#'
#' @author Dominique Makowski
#'
#' @importFrom stats pnorm var approx rchisq
#' @import ggplot2
#' @export
crawford.test <- function(patient, controls=NULL, mean=NULL, sd=NULL, n=NULL, CI=95, iter=10000, color="#2196F3", color_CI="#E91E63", color_score="black", color_size=2) {

  if(is.null(controls)){
    if(TRUE %in% sapply(c(mean, sd, n), is.null)){
      stop("Please provide either controls or mean, sd and n.")
    }
    sample_mean <- mean
    sample_sd <- sd
    sample_var <- sd^2
  } else{
    sample_mean <- mean(controls)
    sample_var <- var(controls)
    sample_sd <- sd(controls)
    n <- length(controls)
  }
  degfree <- n - 1


# Computation -------------------------------------------------------------


  pvalues <- c()
  for(i in 1:iter){
    # step 1
    psi <- rchisq(1, df=degfree, ncp = 0)
    o <- (n - 1) * sample_var / psi

    # step 2
    z <- rnorm(1, 0, 1)
    u <- sample_mean + z * sqrt((o/n))

    # step 3
    z_patient <- (patient - u) / sqrt(o)
    p <- 2 * (1-pnorm(abs(z_patient), lower.tail = TRUE))  # One-tailed p-value
    pvalues <- c(pvalues, p)
  }


# Point estimates ---------------------------------------------------------

  # Two sided
  p_2 <- mean(pvalues)
  CI_2 <- hdi(pvalues, prob = CI/100)

  # One sided
  p_1 <- p/2
  CI_1 <- sort(pvalues)[iter*(100-CI)/100]



# Text --------------------------------------------------------------------
  # One-tailed p value
  if (p_2 > .05 & p_1 < .05) {
    one_tailed <- paste0(
      " However, the null hypothesis of no difference can be rejected at a one-tailed 5% significance level (one-tailed p ",
      format_p(p_1),
      ")."
    )
  } else {
    one_tailed <- ""
  }

  p_interpretation <- ifelse(p_2 < 0.05, " significantly ", " not significantly ")
  direction <- ifelse(patient - sample_mean < 0, " lower than ", " higher than ")


  text <- paste0(
    "The Bayesian test for single case assessment (Crawford, Garthwaite, 2007) suggests that the patient's score (",
    format_digit(patient),
    ") is",
    p_interpretation,
    "different from the controls (M = ",
    format_digit(sample_mean),
    ", SD = ",
    format_digit(sample_sd),
    ", p ",
    format_p(p_2),
    ").",
    one_tailed,
    " The patient's score is",
    direction,
    format_digit((1 - p_1) * 100),
    "% (95% CI [",
    paste(format_digit(sort(c((1 - CI_2$values$HDImin) * 100, (1 - CI_2$values$HDImax) * 100))), collapse = ", "),
    "]) of the control population."
  )



# Store values ------------------------------------------------------------

  values <- list(controls_mean=sample_mean,
                 controls_sd=sample_sd,
                 controls_var=sample_var,
                 controls_sd=sample_sd,
                 controls_n=n,
                 text=text,
                 p=p_2,
                 CI_lower=CI_2$values$HDImin,
                 CI_higher=CI_2$values$HDImax,
                 CI_onesided=CI_1)

  summary <- data.frame(controls_mean=sample_mean,
                        controls_sd=sample_sd,
                        controls_n=n,
                        p=p_2,
                        CI_lower=CI_2$values$HDImin,
                        CI_higher=CI_2$values$HDImax)

  if(is.null(controls)){
    controls <- rnorm_perfect(n, sample_mean, sample_sd)
  }


# Plot --------------------------------------------------------------------
  if(patient - sample_mean < 0){
    lower <- CI_2$values$HDImin
    upper <- CI_2$values$HDImax
  } else{
    lower <- (1-CI_2$values$HDImax)
    upper <- (1-CI_2$values$HDImin)
  }

  plot <- density(controls) %>%
    approx(n = 10000) %>%
    as.data.frame() %>%
    mutate_(area = "dplyr::between(x, x[round(lower*10000)+1], x[round(upper*10000)+1])") %>%
    ggplot() +
    geom_ribbon(aes_string(x = "x", ymin=0, ymax = "y", fill = "area")) +
    geom_vline(xintercept=patient, colour=color_score, size=color_size) +
    scale_fill_manual(values=c(color, color_CI)) +
    xlab("\nScore") +
    ylab("") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank()
    )


  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")
  return(output)

}



























#' Crawford-Howell (1998) frequentist t-test for single-case analysis.
#'
#' Neuropsychologists often need to compare a single case to a small control group. However, the standard two-sample t-test does not work because the case is only one observation. Crawford and Garthwaite (2012) demonstrate that the Crawford-Howell (1998) t-test is a better approach (in terms of controlling Type I error rate) than other commonly-used alternatives.
#' .
#'
#' @param patient Single value (patient's score).
#' @param controls Vector of values (control's scores).
#'
#' @return Returns a data frame containing the t-value, degrees of freedom, and p-value. If significant, the patient is different from the control group.
#'
#' @examples
#' library(psycho)
#'
#' crawford.test.freq(patient = 10, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#' crawford.test.freq(patient = 7, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
#'
#' @author Dan Mirman, Dominique Makowski
#'
#' @importFrom stats pt sd
#' @export
crawford.test.freq <- function(patient, controls) {
  tval <- (patient - mean(controls)) / (sd(controls) * sqrt((length(controls) + 1) / length(controls)))

  degfree <- length(controls) - 1

  pval <- 2 * (1 - pt(abs(tval), df = degfree)) # One-tailed p-value

  # One-tailed p value
  if (pval > .05 & pval / 2 < .05) {
    one_tailed <- paste0(
      " However, the null hypothesis of no difference can be rejected at a one-tailed 5% significance level (one-tailed p ",
      format_p(pval / 2),
      ")."
    )
  } else {
    one_tailed <- ""
  }


  p_interpretation <- ifelse(pval < 0.05, " significantly ", " not significantly ")
  t_interpretation <- ifelse(tval < 0, " lower than ", " higher than ")

  text <- paste0(
    "The Crawford-Howell (1998) t-test suggests that the patient's score (",
    format_digit(patient),
    ") is",
    p_interpretation,
    "different from the controls (M = ",
    format_digit(mean(controls)),
    ", SD = ",
    format_digit(sd(controls)),
    ", t(",
    degfree,
    ") = ",
    format_digit(tval),
    ", p ",
    format_p(pval),
    ").",
    one_tailed,
    " The patient's score is",
    t_interpretation,
    format_digit((1 - pval) * 100),
    "% of the control population."
  )

  values <- list(text=text,
                 p=pval,
                 df=degfree,
                 t=tval)
  summary <- data.frame(t = tval, df = degfree, p = pval)
  plot <- "Not available yet"


  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")
  return(output)
}

