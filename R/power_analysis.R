#' Power analysis for fitted models.
#'
#' Compute the n models based on n sampling of data.
#'
#' @param fit A lm or stanreg model.
#' @param n_max Max sample size.
#' @param n_min Min sample size. If null, take current nrow.
#' @param step Increment of the sequence.
#' @param n_batch Number of iterations at each sample size.
#' @param groups Grouping variable name (string) to preserve proportions. Can be a list of strings.
#' @param verbose Print progress.
#'
#' @return A dataframe containing the summary of all models for all iterations.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#' library(psycho)
#'
#' fit <- lm(Sepal.Length ~ Sepal.Width, data=iris)
#'
#' results <- power_analysis(fit, n_max=300, n_min=100, step=5, n_batch=20)
#'
#' results %>%
#'   filter(Variable=="Sepal.Width") %>%
#'   select(n, p) %>%
#'   group_by(n) %>%
#'   summarise(p_median = median(p),
#'             p_mad = mad(p)) %>%
#'   ggplot(aes(x=n, y=p_median)) +
#'   geom_hline(aes(yintercept=0.05), color="red", linetype="dashed") +
#'    geom_line() +
#'    geom_ribbon(aes(ymin=p_median-p_mad, ymax=p_median+p_mad), alpha=0.2) +
#'    geom_smooth(method="lm", formula = y ~ poly(x, 2))
#'  }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats model.frame
#' @import dplyr
#' @export
power_analysis <- function(fit, n_max, n_min=NULL, step=1, n_batch=1, groups=NULL, verbose=TRUE){

  # Parameters
  df <- model.frame(fit)

  if(is.null(n_min)){
    n_min <- nrow(df)
  }


  results <- data.frame()
  for(n in seq(n_min, n_max, step)){

    for(batch in 1:n_batch){

      # Progress
      if(verbose==TRUE){
        cat(".")
      }


      # Sample data.frame
      if(!is.null(groups)){
        newdf <- df %>%
          group_by_(groups) %>%
          dplyr::sample_frac(n/nrow(df), replace=TRUE)
      }else{
        newdf <- dplyr::sample_frac(df, n/nrow(df), replace=TRUE)
      }

      # Fit new model
      newfit <- update(fit, data=newdf)
      newfit <- analyze(newfit, CI=90)

      # Store results
      newresults <- summary(newfit)
      newresults$n <- n
      newresults$batch <- batch
      results <- rbind(results, newresults)
    }

    # Progress
    if(verbose==TRUE){
      n_iterations <- (n_max-n_min)*n_batch
      cat(paste0(format_digit(round((n-n_min)/(n_max-n_min)*100)), "%\n"))
    }


  }
  return(results)
}

