#' Multiple Correlations.
#'
#' Compare a given score to a parent population.
#'
#' @param df The score.
#' @param df2 The general population's mean.
#' @param type A character string indicating which correlation type is to be computed. One of "full" (default"), "partial" or "semi" for semi-partial correlations.
#' @param method A character string indicating which correlation coefficient is to be computed. One of "pearson" (default), "kendall", or "spearman" can be abbreviated.
#' @param adjust What adjustment for multiple tests should be used? ("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"). See \link[stats]{p.adjust} for details about why to use "holm" rather than "bonferroni").
#'
#' @return output
#'
#' @examples
#' assess(124, mean=100, sd=15)
#'
#' @author Dominique Makowski, \url{https://dominiquemakowski.github.io/}
#'
#' @importFrom stats na.omit
#' @importFrom psych corr.test
#' @importFrom psych corr.p
#' @import corrplot
#' @import ppcor
#' @export
correlation <- function(df, df2=NULL, type="full", method="pearson", adjust="holm") {

  # Processing
  # -------------------

  # Remove non numeric
  df <- df[ , sapply(df, is.numeric)]

  # Compute r coefficients
  if (type=="full"){
    r <- psych::corr.test(df, y=df2, use="pairwise", method=method)$r
  } else{
    df <- cbind(df, df2)
    df <- stats::na.omit(df)  # enable imputation
    if (type=="partial"){
      r <- ppcor::pcor(df, method=method)$estimate
    }
    if (type=="semi"){
      r <- ppcor::spcor(df, method=method)$estimate
    }
  }


  # Get P values
  n <- nrow(df)
  p <- psych::corr.p(r, n, adjust=adjust)$p
  ci <- psych::corr.p(r, n, adjust=adjust)$ci


  # Format into a table
  p.mat <- matrix(p, ncol=ncol(r), dimnames=list(names(df), names(df)))
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  ## trunctuate the matrix that holds the correlations to two decimal
  r_format <- format(round(cbind(rep(-1.11, ncol(df)), r), 2))[,-1]
  ## build a new matrix that includes the correlations with their apropriate stars
  table <- matrix(paste(r_format, mystars, sep=""), ncol=ncol(df))
  diag(table) <- paste(diag(r_format), " ", sep="")
  rownames(table) <- colnames(df)
  colnames(table) <- paste(colnames(df), "", sep="")
  ## remove upper triangle
  table <- as.matrix(table)
  table[upper.tri(table, diag = TRUE)] <- NA
  table <- as.data.frame(table)
  ## remove last column and return the matrix (which is now a data frame)
  table <- cbind(table[1:length(table)-1])


  # Values
  # -------------
  values <- list(r=r, p=p, ci=ci, table=table)

  # Summary
  # -------------
  summary <- table

  # Text
  # -------------
  text <- table


  # Plot
  # -------------
  plot <- corrplot::corrplot.mixed(r, lower="ellipse", upper="number", order = "hclust", p.mat = p, sig.level = 0.05, insig="n", tl.pos="lt")


  # Output
  # -------------
  output <- list(text=text, plot=plot, summary=summary, values=values)

  class(output) <- c("psychobject", "list")
  return(output)
}
