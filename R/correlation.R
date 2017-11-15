#' Multiple Correlations.
#'
#' Compare a given score to a parent population.
#'
#' @param df The dataframe
#' @param df2 Optional dataframe to correlate with the first one.
#' @param type A character string indicating which correlation type is to be
#'   computed. One of "full" (default), "partial" or "semi" for semi-partial
#'   correlations.
#' @param method A character string indicating which correlation coefficient is
#'   to be computed. One of "pearson" (default), "kendall", or "spearman" can be
#'   abbreviated.
#' @param adjust What adjustment for multiple tests should be used? ("holm",
#'   "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). See
#'   \link[stats]{p.adjust} for details about why to use "holm" rather than
#'   "bonferroni").
#'
#' @return output
#'
#' @examples
#' df <- attitude
#'
#' # Normal correlations
#' results <- psycho::correlation(df)
#' print(results)
#' plot(results)
#'
#' # Partial correlations with correction
#' results <- psycho::correlation(df, type="partial",
#'                                    method="spearman",
#'                                    adjust="holm")
#' print(results)
#' plot(results)
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom stats na.omit p.adjust
#' @importFrom psych corr.test
#' @importFrom ggplot2 theme element_text
#' @import ggcorrplot
#' @import ppcor
#' @export
correlation <- function(df,
                        df2 = NULL,
                        type = "full",
                        method = "pearson",
                        adjust = "holm") {

  # Processing
  # -------------------

  # Remove non numeric
  df <- df[ , sapply(df, is.numeric)]
  if (is.null(df2) == FALSE) {
    df2 <- df2[ , sapply(df2, is.numeric)]
  }
  # Compute r coefficients
  if (type == "full") {
    corr <- psych::corr.test(df, y = df2, use = "pairwise", method = method, adjust="none")
    r <- corr$r
    p <- corr$p
    t <- corr$t
    ci <- corr$ci
    ci.adj <- corr$ci.adj

    r <- psych::corr.test(df, y = df2, use = "pairwise", method = method)$r
  } else{

    if (is.null(df2) == FALSE) {
      df <- cbind(df, df2)
    }

    df <- stats::na.omit(df)  # enable imputation
    if (type == "semi") {corr <- ppcor::spcor(df, method = method)}
    else if (type == "partial") {corr <- ppcor::pcor(df, method = method)}
    else {
      warning("type parameter must be 'full', 'semi' or 'partial'")
      return()
    }
    r <- corr$estimate
    p <- corr$p.value
    t <- corr$statistic
    ci <- "Not available for partial and semipartial correlations."
    ci.adj <- "Not available for partial and semipartial correlations."
  }


  # Adjust P values
  n <- nrow(df)
  if (adjust != "none"){
    if ((type == "full" & is.null(df2)==F)|(type == "semi")) {
      p <- p.adjust(p, method=adjust)
    } else{
      p[lower.tri(p)] <- p.adjust(p[lower.tri(p)], method=adjust, n=choose(nrow(p), 2))
      p[upper.tri(p)] <- p.adjust(p[upper.tri(p)], method=adjust, n=choose(nrow(p), 2))
    }
  }



  # Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***",
               ifelse(p < .01, "** ",
                 ifelse(p < .05, "* ", " ")))

  # trunctuate the matrix that holds the correlations to two decimal
  r_format <- format(round(cbind(rep(-1.11, ncol(df)), r), 2))[,-1]
  # build a new correlation matrix with significance stars
  table <- matrix(paste(r_format, mystars, sep = ""), ncol = ncol(df))

  # Format
  if ((type == "full" & is.null(df2)==F)|(type == "semi")) {
    if (type == "semi"){
      rownames(table) <- colnames(df)
    } else{
      rownames(table) <- colnames(df2)
    }
    colnames(table) <- paste(colnames(df), "", sep = "")
    table <- as.matrix(table)
    table <- as.data.frame(table)
  } else {
    diag(table) <- paste(diag(r_format), " ", sep = "")
    rownames(table) <- colnames(df)
    colnames(table) <- paste(colnames(df), "", sep = "")
    table <- as.matrix(table)
    table[upper.tri(table, diag = TRUE)] <- NA  # remove upper triangle
    table <- as.data.frame(table)
    table <- cbind(table[1:length(table) - 1])  # remove last column and return the matrix (which is now a data frame)
  }



  # Values
  # -------------
  values <- list(r = r, p = p, t = t, ci = ci, ci.adj = ci.adj, table = table)

  # Summary
  # -------------
  summary <- table

  # Text
  # -------------
  text <- table


  # Plot
  # -------------
  if (is.null(df2)==F & type=="full"){
    corr <- psych::corr.test(cbind(df, df2), use = "pairwise", method = method, adjust="none")
    r <- corr$r
    p <- corr$p
    p[lower.tri(p)] <- p.adjust(p[lower.tri(p)], method=adjust, n=choose(nrow(p), 2))
    p[upper.tri(p)] <- p.adjust(p[upper.tri(p)], method=adjust, n=choose(nrow(p), 2))
    warning("Due to the presence of two dataframes, the plot might be incorrect. Consider with caution.")
  }

  if (type=="semi"){
    plot <- ggcorrplot::ggcorrplot(r,
                                   title = paste("A ", type, "'s correlation matrix (correction: ", adjust, ")\n", sep = ""),
                                   method = "circle",
                                   type="full",
                                   colors=c("#E91E63", "white", "#03A9F4"),
                                   hc.order = TRUE,
                                   p.mat = p,
                                   insig="pch",
                                   legend.title="",
                                   lab=FALSE) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.7))

  } else{
    plot <- ggcorrplot::ggcorrplot(r,
                                   title = paste("A ", type, "'s correlation matrix (correction: ", adjust, ")\n", sep = ""),
                                   method = "circle",
                                   type="lower",
                                   colors=c("#E91E63", "white", "#03A9F4"),
                                   hc.order = TRUE,
                                   p.mat = p,
                                   insig="pch",
                                   legend.title="",
                                   lab=FALSE) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.7))
  }



  # Output
  # -------------
  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
