#' Analyze aov objects.
#'
#' Analyze aov objects.
#'
#' @param x aov object.
#' @param effsize_rules Grid for effect size interpretation. See \link[=interpret_omega_sq]{interpret_omega_sq}.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' df <- psycho::affective
#'
#' x <- aov(df$Tolerating ~ df$Salary)
#' x <- aov(df$Tolerating ~ df$Salary * df$Sex)
#'
#' summary(analyze(x))
#' print(analyze(x))
#' }
#'
#'
#' @references
#' \itemize{
#'  \item{Levine, T. R., & Hullett, C. R. (2002). Eta squared, partial eta squared, and misreporting of effect size in communication research. Human Communication Research, 28(4), 612-625.}
#'  \item{JPierce, C. A., Block, R. A., & Aguinis, H. (2004). Cautionary note on reporting eta-squared values from multifactor ANOVA designs. Educational and psychological measurement, 64(6), 916-924.}
#'}
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import broom
#'
#' @export
analyze.aov <- function(x, effsize_rules="field2013", ...) {

  if(!"aov" %in% class(x)){
    if(!"Residuals" %in% row.names(x)){
      stop("Cannot deal with this object. You should use mixed-models.")
    }
  } else{
    if(!is.null(x$Within)){
      stop("Cannot deal with repeated measures. Please use mixed-models.")
    }
  }




  # Processing
  # -------------


  all_values <- x %>%
    broom::tidy() %>%
    dplyr::full_join(data.frame("Omega" = omega_sq(x, partial=TRUE)) %>%
                       tibble::rownames_to_column("term"), by="term") %>%
    mutate_("Effect_Size" = "interpret_omega_sq(Omega, rules = 'field2013')") %>%
    rename_("Effect" = "term",
            "Sum_Squares" = "sumsq",
            "Mean_Square" = "meansq",
            "F" = "statistic",
            "p" = "p.value")

  varnames <- all_values$Effect
  df_residuals <- all_values[all_values$Effect == "Residuals", ]$df

  values <- list()
  for(var in varnames){
    values[[var]] <- list()
    current_values <- dplyr::filter_(all_values, "Effect == var")
    values[[var]]$df <- current_values$df
    values[[var]]$Sum_Squares <- current_values$Sum_Squares
    values[[var]]$meansq <- current_values$Mean_Square
    values[[var]]$F <- current_values$F
    values[[var]]$p <- current_values$p
    values[[var]]$Omega <- current_values$Omega
    values[[var]]$Effect_Size <- current_values$Effect_Size

    if(var != "Residuals"){
      if(current_values$p < .05){
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
                                   format_digit(current_values$F),
                                   ", p ",
                                   format_p(current_values$p, stars=FALSE),
                                   ") and can be considered as ",
                                   current_values$Effect_Size,
                                   " (Partial Omega-squared = ",
                                   format_digit(current_values$Omega),
                                   ").")
      }
    }

  # Summary
  # -------------
  summary <- all_values %>%
    dplyr::select_("-Sum_Squares", "-Mean_Square")

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








#' Partial Omega Squared.
#'
#' Partial Omega Squared.
#'
#' @param x aov object.
#' @param partial Return partial omega squared.
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
#' omega_sq(x)
#'
#' @seealso http://stats.stackexchange.com/a/126520
#'
#' @author Arnoud Plantinga
#' @importFrom stringr str_trim
#' @export
omega_sq <- function(x, partial=TRUE){
  if("aov" %in% class(x)){
    summary_aov <- summary(x)[[1]]
  } else{
    summary_aov <- x
  }
  residRow   <- nrow(summary_aov)
  dfError    <- summary_aov[residRow,1]
  msError    <- summary_aov[residRow,3]
  nTotal     <- sum(summary_aov$Df)
  dfEffects  <- summary_aov[1:{residRow-1},1]
  ssEffects  <- summary_aov[1:{residRow-1},2]
  msEffects  <- summary_aov[1:{residRow-1},3]
  ssTotal    <- rep(sum(summary_aov[1:residRow, 2]), 3)
  Omegas <- abs((ssEffects - dfEffects*msError)/(ssTotal + msError))
  names(Omegas) <- stringr::str_trim(rownames(summary_aov)[1:{residRow-1}])

  partOmegas <- abs((dfEffects*(msEffects-msError)) /
                      (ssEffects + (nTotal -dfEffects)*msError))
  names(partOmegas) <- stringr::str_trim(rownames(summary_aov)[1:{residRow-1}])

  if(partial == TRUE){
    return(partOmegas)
  } else{
    return(Omegas)
  }
}
