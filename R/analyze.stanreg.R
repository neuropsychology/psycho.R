#' Analyze stanreg objects.
#'
#' Analyze stanreg objects.
#'
#' @param x stanreg object.
#' @param CI Credible interval bounds.
#' @param Effect_Size Compute Effect Sizes according to Cohen (1988)? Your outcome variable must be standardized.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' require(rstanarm)
#' fit <- rstanarm::stan_glm(vs ~ mpg * cyl,
#'   data=mtcars,
#'   family = binomial(link = "logit"),
#'   prior=NULL)
#'
#'  analyze(fit)
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @import rstanarm
#' @import tidyr
#' @import dplyr
#' @import ggplot2
#' @importFrom stats quantile
#' @export
analyze.stanreg <- function(x, CI=95, Effect_Size=FALSE, ...) {


  # Processing
  # -------------
  fit <- x

  # Extract posterior distributions
  posteriors <- as.data.frame(fit)

  # Varnames
  varnames <- names(fit$coefficients)
  varnames <- varnames[grepl("b\\[", varnames)==FALSE]


  # Initialize empty values
  values <- list()
  # Loop over all variables
  for (varname in varnames){
    # Extract posterior
    posterior <- posteriors[, varname]

    # Find basic posterior indices
    median <- median(posterior)
    mad <- mad(posterior)
    mean <- mean(posterior)
    sd <- sd(posterior)
    CI_values <- quantile(posterior, c((100-CI)/2/100, 1-(100-CI)/2/100), type=8)

    # Compute MPE
    if (median >= 0){
      MPE <- length(posterior[posterior>=0])/length(posterior)*100
      if (MPE == 100){
        MPE_values <- c(min(posterior), max(posterior))
      } else{
        MPE_values <- c(0, max(posterior))
      }


    } else {
      MPE <- length(posterior[posterior<0])/length(posterior)*100
      if (MPE == 100){
        MPE_values <- c(min(posterior), max(posterior))
      } else{
        MPE_values <- c(min(posterior), 0)
      }
    }



    # Create text
    if (grepl(":", varname)){
      splitted <- strsplit(varname, ":")[[1]]
      if (length(splitted) == 2){
        name <- paste("interaction effect between ",
                      splitted[1], " and ", splitted[2], sep = "")
      } else{
          name <- varname
        }
      } else{
        name <- paste("effect of ", varname, sep="")
    }

    text <- paste("Concerning the ", name, ", there is a probability of ", format_digit(MPE), "% that its coefficient is between ", format_digit(MPE_values[1]), " and ", format_digit(MPE_values[2]), " (Median = ", format_digit(median), ", MAD = ", format_digit(mad), ", Mean = ", format_digit(mean), ", SD = ", format_digit(sd), ", ", CI, "% CI [", format_digit(CI_values[1]), ", ", format_digit(CI_values[2]), "]).", sep="")

    # Store all that
    values[[varname]] <- list(name=varname,
                              median=median,
                              mad=mad,
                              mean=mean,
                              sd=sd,
                              CI_values=CI_values,
                              MPE=MPE,
                              MPE_values=MPE_values,
                              posterior=posterior,
                              text=text)

  }


  # Effect size
  # -------------
  if (Effect_Size==T){
    print("Interpreting effect size following Cohen (1977, 1988)... Make sure your variables were standardized!")

    EffSizes <- data.frame()
    for (varname in varnames){
      posterior <- posteriors[, varname]
    # Compute the probabilities
      verylarge_neg <- length(posterior[posterior <= -1.30])/length(posterior)
      large_neg <- length(posterior[posterior > -1.30 & posterior <= -0.80])/length(posterior)
      medium_neg <- length(posterior[posterior > -0.80 & posterior <= -0.50])/length(posterior)
      small_neg <- length(posterior[posterior > -0.50 & posterior <= -0.20])/length(posterior)
      verysmall_neg <- length(posterior[posterior > -0.20 & posterior < 0])/length(posterior)

      verylarge_pos <- length(posterior[posterior >= 1.30])/length(posterior)
      large_pos <- length(posterior[posterior < 1.30 & posterior >= 0.80])/length(posterior)
      medium_pos <- length(posterior[posterior < 0.80 & posterior >= 0.50])/length(posterior)
      small_pos <- length(posterior[posterior < 0.50 & posterior >= 0.20])/length(posterior)
      verysmall_pos <- length(posterior[posterior < 0.20 & posterior > 0])/length(posterior)

      EffSize <- data.frame(Direction=c("Negative", "Negative", "Negative", "Negative", "Negative", "Positive", "Positive", "Positive", "Positive", "Positive"),
                                Size=c("VeryLarge", "Large", "Medium", "Small", "VerySmall", "VerySmall", "Small", "Medium", "Large", "VeryLarge"),
                                Probability=c(verylarge_neg, large_neg, medium_neg, small_neg, verysmall_neg, verysmall_pos, small_pos, medium_pos, large_pos, verylarge_pos))

      EffSize$Probability[is.na(EffSize$Probability)] <- 0
      EffSize$Variable <- varname

      EffSizes <- rbind(EffSizes, EffSize)

      if(mean(posterior) >= 0){
        opposite_prob <- sum(EffSize$Probability[EffSize$Direction=="Negative"])
        if (length(posterior[posterior > 0])>0){
          opposite_max <- min(posterior[posterior > 0])
        } else{
          opposite_max <- 0
        }
        verylarge <- verylarge_pos
        large <- large_pos
        medium <- medium_pos
        small <- small_pos
        verysmall <- verysmall_pos
      } else{
        opposite_prob <- sum(EffSize$Probability[EffSize$Direction=="Positive"])
        if (length(posterior[posterior > 0])>0){
        opposite_max <- max(posterior[posterior > 0])
        } else{
          opposite_max <- 0
        }
        verylarge <- verylarge_neg
        large <- large_neg
        medium <- medium_neg
        small <- small_neg
        verysmall <- verysmall_neg
      }

      EffSize_text <- paste("Based on Cohen (1988) recommandations, there is a probability of ", round(verylarge*100, 2), "% that this effect size is very large, ", round(large*100, 2), "% that this effect size is large, ", round(medium*100, 2), "% that this effect size is medium, ", round(small*100, 2), "% that this effect size is small, ", round(verysmall*100, 2), "% that this effect is very small and ", round(opposite_prob*100, 2), "% that it has an opposite direction (between 0 and ", signif(opposite_max, 2), ").", sep="")
      values[[varname]]$EffSize <- EffSize
      values[[varname]]$EffSize_text <- EffSize_text

      values[[varname]]$EffSize_VL <- verylarge
      values[[varname]]$EffSize_L <- large
      values[[varname]]$EffSize_M <- medium
      values[[varname]]$EffSize_S <- small
      values[[varname]]$EffSize_VS <- verysmall
      values[[varname]]$EffSize_O <- opposite_prob

    }
    }


  # Summary
  # -------------
  MPEs <- c()
  for (varname in names(values)){
    MPEs <- c(MPEs, values[[varname]]$MPE)
  }
  medians <- c()
  for (varname in names(values)){
    medians <- c(medians, values[[varname]]$median)
  }
  mads <- c()
  for (varname in names(values)){
    mads <- c(mads, values[[varname]]$mad)
  }
  means <- c()
  for (varname in names(values)){
    means <- c(means, values[[varname]]$mean)
  }
  sds <- c()
  for (varname in names(values)){
    sds <- c(sds, values[[varname]]$sd)
  }
  CIs <- c()
  for (varname in names(values)){
    CIs <- c(CIs, values[[varname]]$CI_values)
  }

  summary <- data.frame(Variable=names(values), MPE=MPEs, Median=medians,
                        MAD=mads, Mean=means, SD=sds,
                        CI_lower=CIs[seq(1, length(CIs), 2)],
                        CI_higher=CIs[seq(2, length(CIs), 2)])

  if (Effect_Size==T){
    EffSizes <- data.frame()
    for (varname in names(values)){
      Current <- data.frame(Very_Large = values[[varname]]$EffSize_VL,
                            Large = values[[varname]]$EffSize_L,
                            Medium = values[[varname]]$EffSize_M,
                            Small = values[[varname]]$EffSize_S,
                            Very_Small = values[[varname]]$EffSize_VS,
                            Opposite = values[[varname]]$EffSize_O)
      EffSizes <- rbind(EffSizes, Current)
    }
    summary <- cbind(summary, EffSizes)
  }



  # Text
  # -------------
  # Model
  info <- paste("We fitted a Markov Chain Monte Carlo [type] model to predict [Y] with [X] (formula =", fit$formula, "). Priors were set as follow: [INSERT INFO ABOUT PRIORS].", sep="")

  # Coefs
  coefs_text <- c()
  for (varname in names(values)){
    coefs_text <- c(coefs_text, values[[varname]]$text)
    if (Effect_Size==T){
    coefs_text <- c(coefs_text, values[[varname]]$EffSize_text)
    }
  }
  text <- c(info, coefs_text)



  # Plot
  # -------------
  plot <- posteriors[varnames] %>%
    # select(-`(Intercept)`) %>%
    gather() %>%
    rename_(Variable="key", Coefficient="value") %>%
    ggplot(aes_string(x="Variable", y="Coefficient", fill="Variable")) +
    geom_violin() +
    geom_boxplot(fill="grey", alpha=0.3, outlier.shape=NA) +
    stat_summary(fun.y = mean, geom = "errorbar", aes_string(ymax = "..y..", ymin = "..y.."), width = .75, linetype = "dashed", colour="red") +
    geom_hline(aes(yintercept=0)) +
    theme_classic() +
    coord_flip() +
    scale_fill_brewer(palette="Set1") +
    scale_colour_brewer(palette="Set1")


  output <- list(text=text, plot=plot, summary=summary, values=values)

  class(output) <- c("psychobject", "list")
  return(output)
}
