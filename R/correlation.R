#' Multiple Correlations.
#'
#' Compute different kinds of correlation matrices.
#'
#' @param df The dataframe.
#' @param df2 Optional dataframe to correlate with the first one.
#' @param type A character string indicating which correlation type is to be
#'   computed. One of "full" (default), "partial" (partial correlations),
#'   "semi" (semi-partial correlations), "glasso"
#'   (Graphical lasso- estimation of Gaussian graphical models) or "cor_auto"
#'   (will use the qgraph::cor_auto function to return pychoric or polyserial
#'   correlations if needed).
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
#' @importFrom stats na.omit p.adjust cor
#' @importFrom psych corr.test
#' @importFrom ggplot2 theme element_text
#' @importFrom stringr str_to_title
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
  # N samples
  n <- nrow(df)

  # Remove non numeric
  df <- df[, sapply(df, is.numeric)]
  if (is.null(df2) == FALSE) {
    df2 <- df2[, sapply(df2, is.numeric)]
  }
  # Compute r coefficients
  if (type == "full") {
    corr <- psych::corr.test(df, y = df2, use = "pairwise", method = method, adjust = "none")
    r <- corr$r
    p <- corr$p
    t <- corr$t
    ci <- corr$ci
    ci.adj <- corr$ci.adj

    r <- psych::corr.test(df, y = df2, use = "pairwise", method = method)$r
  } else {
    if (is.null(df2) == FALSE) {
      df <- cbind(df, df2)
    }

    df <- stats::na.omit(df) # enable imputation
    if (type == "semi") {
      corr <- ppcor::spcor(df, method = method)
      r <- corr$estimate
      p <- corr$p.value
      t <- corr$statistic
      ci <- "Not available for partial and semipartial correlations."
      ci.adj <- "Not available for partial and semipartial correlations."
    }
    else if (type == "partial") {
      corr <- ppcor::pcor(df, method = method)
      r <- corr$estimate
      p <- corr$p.value
      t <- corr$statistic
      ci <- "Not available for partial and semipartial correlations."
      ci.adj <- "Not available for partial and semipartial correlations."
    }
    else if (type == "glasso") {
      corr <- qgraph::EBICglasso(cor(df), n, gamma = 0.5)
      r <- corr
      p <- NULL
      t <- NULL
      ci <- "Not available for glasso estimation."
      ci.adj <- "Not available for glasso estimation."
    }
    else if (type == "cor_auto") {
      corr <- qgraph::cor_auto(df, forcePD = F)
      r <- corr
      p <- NULL
      t <- NULL
      ci <- "Not available for cor_auto estimation."
      ci.adj <- "Not available for cor_auto estimation."
    }
    else {
      warning("type parameter must be 'full', 'semi', 'partial', 'glasso' or 'cor_auto'")
      return()
    }
  }



  # Adjust P values
  if (is.null(p) == F) {
    if (adjust != "none") {
      if ((type == "full" & is.null(df2) == F) | (type == "semi")) {
        p <- p.adjust(p, method = adjust)
      } else {
        p[lower.tri(p)] <- p.adjust(p[lower.tri(p)], method = adjust, n = choose(nrow(p), 2))
        p[upper.tri(p)] <- p.adjust(p[upper.tri(p)], method = adjust, n = choose(nrow(p), 2))
      }
    }
  }




  # Values
  # -------------
  values <- list(r = r, p = p, t = t, ci = ci, ci.adj = ci.adj, n = n)





  # Summary
  # -------------

  # Define notions for significance levels; spacing is important.
  if (is.null(p) == F) {
    mystars <- ifelse(p < .001, "***",
      ifelse(p < .01, "** ",
        ifelse(p < .05, "* ", " ")
      )
    )
  } else {
    mystars <- ""
  }


  # trunctuate the matrix that holds the correlations to two decimal
  r_format <- format(round(cbind(rep(-1.11, ncol(df)), r), 2))[, -1]
  # build a new correlation matrix with significance stars
  table <- matrix(paste(r_format, mystars, sep = ""), ncol = ncol(r))


  # Format
  rownames(table) <- colnames(df)
  if (isSymmetric(r)) {
    diag(table) <- paste(diag(r_format), " ", sep = "")
    colnames(table) <- paste(colnames(df), "", sep = "")
    table[upper.tri(table, diag = TRUE)] <- "" # remove upper triangle
    table <- as.data.frame(table)
    summary <- cbind(table[1:length(table) - 1]) # remove last column and return the matrix (which is now a data frame)
  } else {
    if (is.null(df2)) {
      colnames(table) <- paste(colnames(df), "", sep = "")
    } else {
      if (type == "semi") {
        colnames(table) <- paste(colnames(df), "", sep = "")
      } else {
        colnames(table) <- paste(colnames(df2), "", sep = "")
      }
    }
    table <- as.data.frame(table)
    summary <- table
  }




  # Text
  # -------------
  sentences <- c()
  for (row in 1:nrow(r)) {
    for (col in 1:ncol(r)) {
      if (as.matrix(table)[row, col] == "") next # skip iteration and go to next iteration

      val_r <- as.matrix(r)[row, col]
      val_t <- tryCatch({
        as.matrix(t)[row, col]
      }, error = function(e) {
        "NA"
      })
      val_p <- tryCatch({
        as.matrix(p)[row, col]
      }, error = function(e) {
        "NA"
      })
      var1 <- colnames(r)[col]
      var2 <- row.names(r)[row]

      if (is.numeric(val_p) & val_p <= .05) {
        significance <- "significant and "
      } else if (is.numeric(val_p) & val_p > .05) {
        significance <- "non significant and "
      } else {
        significance <- ""
      }

      if (abs(val_r) < .30) {
        strength <- "weak"
      } else if (abs(val_r) < .5) {
        strength <- "moderate"
      } else {
        strength <- "strong"
      }


      if (val_r < 0) {
        direction <- "positive"
      } else {
        direction <- "negative"
      }

      sentence <- paste0(
        "   - ",
        var1,
        " - ",
        var2,
        ":   ",
        "Results of the ",
        stringr::str_to_title(method),
        " correlation showed a ",
        significance,
        strength,
        " ",
        direction,
        " association between ",
        var1,
        " and ",
        var2,
        " (r(",
        n - 2,
        ") = ",
        psycho::format_digit(val_r),
        ", p ",
        psycho::format_p(val_p),
        ")."
      )

      sentences <- c(sentences, sentence)
    }
  }

  sentences <- c(paste0(
    stringr::str_to_title(method),
    " ",
    stringr::str_to_title(type),
    " Correlation (p value correction: ",
    adjust,
    "):"
  ), sentences)

  text <- sentences




  # Plot
  # -------------
  if (is.null(df2) == F & type == "full") {
    corr <- psych::corr.test(cbind(df, df2), use = "pairwise", method = method, adjust = "none")
    r <- corr$r
    p <- corr$p
    p[lower.tri(p)] <- p.adjust(p[lower.tri(p)], method = adjust, n = choose(nrow(p), 2))
    p[upper.tri(p)] <- p.adjust(p[upper.tri(p)], method = adjust, n = choose(nrow(p), 2))
    warning("Due to the presence of two dataframes, the plot might be incorrect. Consider with caution.")
  }

  if (type == "semi") {
    plot <- ggcorrplot::ggcorrplot(
      r,
      title = paste("A ", type, "'s correlation matrix (correction: ", adjust, ")\n", sep = ""),
      method = "circle",
      type = "full",
      colors = c("#E91E63", "white", "#03A9F4"),
      hc.order = TRUE,
      p.mat = p,
      insig = "pch",
      legend.title = "",
      lab = FALSE
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.7))
  } else {
    plot <- ggcorrplot::ggcorrplot(
      r,
      title = paste("A ", type, "'s correlation matrix (correction: ", adjust, ")\n", sep = ""),
      method = "circle",
      type = "lower",
      colors = c("#E91E63", "white", "#03A9F4"),
      hc.order = TRUE,
      p.mat = p,
      insig = "pch",
      legend.title = "",
      lab = FALSE
    ) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.7))
  }



  # Output
  # -------------
  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
