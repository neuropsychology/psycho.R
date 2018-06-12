#' Analyze aov objects.
#'
#' Analyze aov objects.
#'
#' @param x aov object.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#'
#' df <- psycho::affective
#'
#' x <- aov(df$Tolerating ~ df$Salary)
#' x <- aov(df$Tolerating ~ df$Salary * df$Sex)
#'
#' summary(analyze(x))
#'
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import broom
#'
#' @export
analyze.aov <- function(x, ...) {

  # TODO: this must be enhanced!

  # Summary
  # -------------
  summary <- broom::tidy(x)


  # Processing
  # -------------
  varnames <- summary$term
  df_residuals <- x$df.residual

  values <- list()
  for(var in varnames){
    values[[var]] <- list()
    current_values <- dplyr::filter_(summary, "term == var")
    values[[var]]$df <- current_values$df
    values[[var]]$sumsq <- current_values$sumsq
    values[[var]]$meansq <- current_values$meansq
    values[[var]]$f <- current_values$statistic
    values[[var]]$p <- current_values$p.value

    if(var != "Residuals"){
      if(current_values$p.value < .05){
        significance <- "significant"
      } else{
        significance <- "not significant"
      }

      if(grepl(":", var)){
        effect <- "interaction between"
        varname <- stringr::str_replace_all(var, ":", " and ")
      } else{
        varname <- var
        effect <- "effect of"
      }

      values[[var]]$text <- paste0("The ",
                                   effect,
                                   " ",
                                   varname,
                                   " is ",
                                   significance,
                                   " (F(",
                                   current_values$df,
                                   ", ",
                                   df_residuals,
                                   ") = ",
                                   format_digit(current_values$statistic),
                                   ", p ",
                                   format_p(current_values$p.value, stars=FALSE),
                                   ").")
      }
    }


  # Text
  # -------------
  text <- c()
  for(var in varnames[varnames != "Residuals"]){
    text <- c(text, paste("   -", values[[var]]$text))
  }


  # Plot
  # -------------
  plot <- "Not available yet"

  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
