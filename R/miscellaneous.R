
#' Check if a dataframe is standardized.
#'
#' Check if a dataframe is standardized.
#'
#' @param df A dataframe.
#' @param tol The error treshold.
#'
#' @examples
#' library(psycho)
#' library(effectsize)
#'
#' df <- psycho::affective
#' is.standardized(df)
#'
#' dfZ <- effectsize::standardize(df)
#' is.standardized(dfZ)
#' @return bool.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
is.standardized <- function(df, tol = 0.1) {
  dfZ <- effectsize::standardize(df)
  dfZnum <- dfZ[sapply(dfZ, is.numeric)]

  dfnum <- dfZ[sapply(df, is.numeric)]

  error <- as.matrix(dfnum) - as.matrix(dfZnum)
  error <- as.data.frame(error)
  names(error) <- names(dfnum)

  error_mean <- error %>%
    summarise(across(everything(), mean))

  if (TRUE %in% as.character(error_mean[1, ] > tol)) {
    standardized <- FALSE
  } else {
    standardized <- TRUE
  }
  return(standardized)
}









# model_to_priors <- function(fit, autoscale = FALSE) {
#   posteriors <- as.data.frame(fit)
#
#   # Varnames
#   varnames <- names(posteriors)
#   varnames <- varnames[grepl("b\\[", varnames) == FALSE]
#
#   fixed_effects <- names(fit$coefficients)
#   fixed_effects <- fixed_effects[grepl("b\\[", fixed_effects) == FALSE]
#   fixed_effects <- fixed_effects[fixed_effects != "(Intercept)"]
#
#   # Get priors
#   prior_intercept <- list()
#   priors <- list()
#   prior_aux <- list()
#   for (prior in varnames) {
#     if (prior == "(Intercept)") {
#       prior_intercept$mean <- mean(posteriors[[prior]])
#       prior_intercept$sd <- sd(posteriors[[prior]])
#     } else if (prior %in% fixed_effects) {
#       priors[[prior]] <- list()
#       priors[[prior]]$mean <- mean(posteriors[[prior]])
#       priors[[prior]]$sd <- sd(posteriors[[prior]])
#     } else {
#       prior_aux[[prior]] <- list()
#       prior_aux[[prior]]$mean <- mean(posteriors[[prior]])
#       prior_aux[[prior]]$sd <- sd(posteriors[[prior]])
#     }
#   }
#
#
#   prior_intercept <- rstanarm::normal(
#     prior_intercept$mean,
#     prior_intercept$sd,
#     autoscale = autoscale
#   )
#   prior <- .format_priors(priors, autoscale = autoscale)
#   prior_aux <- .format_priors(prior_aux, autoscale = autoscale)
#
#   return(list(prior_intercept = prior_intercept, prior = prior, priox_aux = prior_aux))
# }



# .format_priors <- function(priors, autoscale = FALSE) {
#   prior_mean <- data.frame(priors) %>%
#     select(contains("mean")) %>%
#     tidyr::gather() %>%
#     select_("value") %>%
#     pull()
#
#   prior_sd <- data.frame(priors) %>%
#     select(contains("sd")) %>%
#     tidyr::gather() %>%
#     select_("value") %>%
#     pull()
#
#   prior <- rstanarm::normal(
#     prior_mean,
#     prior_sd,
#     autoscale = autoscale
#   )
# }












#' Transform z score to percentile.
#'
#' @param z_score Z score.
#'
#' @examples
#' library(psycho)
#' percentile(-1.96)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats pnorm
#' @export
percentile <- function(z_score) {
  perc <- pnorm(z_score) * 100
  return(perc)
}



#' Transform a percentile to a z score.
#'
#' @param percentile Percentile
#'
#' @examples
#' library(psycho)
#' percentile_to_z(95)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats pnorm
#' @export
percentile_to_z <- function(percentile) {
  z <- qnorm(percentile / 100)
  return(z)
}









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
#' @param CI Confidence level.
#'
#' @return A dataframe containing the summary of all models for all iterations.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(psycho)
#'
#' fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
#'
#' results <- power_analysis(fit, n_max = 300, n_min = 100, step = 5, n_batch = 20)
#'
#' results %>%
#'   filter(Variable == "Sepal.Width") %>%
#'   select(n, p) %>%
#'   group_by(n) %>%
#'   summarise(
#'     p_median = median(p),
#'     p_mad = mad(p)
#'   )
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats model.frame update
#' @import dplyr
#' @export
power_analysis <- function(fit, n_max, n_min = NULL, step = 1, n_batch = 1, groups = NULL, verbose = TRUE, CI = 90) {

  # Parameters
  df <- model.frame(fit)

  if (is.null(n_min)) {
    n_min <- nrow(df)
  }


  results <- data.frame()
  for (n in seq(n_min, n_max, step)) {
    for (batch in 1:n_batch) {

      # Progress
      if (verbose == TRUE) {
        cat(".")
      }


      # Sample data.frame
      if (!is.null(groups)) {
        newdf <- df %>%
          group_by(pick(all_of(groups))) %>%
          dplyr::sample_frac(n / nrow(df), replace = TRUE)
      } else {
        newdf <- dplyr::sample_frac(df, n / nrow(df), replace = TRUE)
      }

      # Fit new model
      newfit <- update(fit, data = newdf)
      newresults <- parameters::model_parameters(newfit, ci = CI / 100)

      # Store results
      newresults$n <- n
      newresults$batch <- batch
      results <- rbind(results, newresults)
    }
    # Progress
    if (verbose == TRUE) {
      cat(paste0(insight::format_value(round((n - n_min) / (n_max - n_min) * 100)), "%\n"))
    }
  }
  return(results)
}


















#' Golden Ratio.
#'
#' Returns the golden ratio (1.618034...).
#'
#' @param x A number to be multiplied by the golden ratio. The default (x=1) returns the value of the golden ratio.
#'
#' @examples
#' library(psycho)
#'
#' golden()
#' golden(8)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
golden <- function(x = 1) {
  return(x * (1 + sqrt(5)) / 2)
}









#' Find season of dates.
#'
#' Returns the season of an array of dates.
#'
#' @param dates Array of dates.
#' @param winter month-day of winter solstice.
#' @param spring month-day of spring equinox.
#' @param summer month-day of summer solstice.
#' @param fall month-day of fall equinox.
#'
#' @return season
#'
#' @examples
#' library(psycho)
#'
#' dates <- c("2012-02-15", "2017-05-15", "2009-08-15", "1912-11-15")
#' find_season(dates)
#' @author Josh O'Brien
#'
#' @seealso
#' https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to
#'
#' @export
find_season <- function(dates, winter = "12-21", spring = "3-20", summer = "6-21", fall = "9-22") {
  WS <- as.Date(paste0("2012-", winter), format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date(paste0("2012-", spring), format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date(paste0("2012-", summer), format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date(paste0("2012-", fall), format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(as.character(dates), format = "2012-%m-%d"))

  season <- ifelse(d >= WS | d < SE, "Winter",
                   ifelse(d >= SE & d < SS, "Spring",
                          ifelse(d >= SS & d < FE, "Summer", "Fall")
                   )
  )
  season
}








#' Fuzzy string matching.
#'
#' @param x Strings.
#' @param y List of strings to be matched.
#' @param value Return value or the index of the closest string.
#' @param step Step by which decrease the distance.
#' @param ignore.case if FALSE, the pattern matching is case sensitive and if TRUE, case is ignored during matching.
#'
#' @examples
#' library(psycho)
#' find_matching_string("Hwo rea ouy", c("How are you", "Not this word", "Nice to meet you"))
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_matching_string <- function(x, y, value = TRUE, step = 0.1, ignore.case = TRUE) {
  z <- c()
  for (i in seq_len(length(x))) {
    s <- x[i]
    distance <- 0.99
    closest <- agrep(s, y, max.distance = distance, value = value, ignore.case = ignore.case)

    while (length(closest) != 1) {
      closest <- agrep(s, closest, max.distance = distance, value = value, ignore.case = ignore.case)
      distance <- distance - step
      if (distance < 0) {
        warning(paste0("Couldn't find matching string for '", s, "'. Try lowering the step parameter."))
        closest <- s
      }
    }
    z <- c(z, closest)
  }
  z
}





