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
#' @param i_am_cheating Set to TRUE to run many uncorrected correlations.
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
#' @importFrom stats na.omit p.adjust cor runif
#' @importFrom psych corr.test
#' @importFrom ggplot2 theme element_text
#' @importFrom stringr str_to_title
#' @import ggcorrplot
#' @import ppcor
#' @import dplyr
#' @export
correlation <- function(df,
                        df2 = NULL,
                        type = "full",
                        method = "pearson",
                        adjust = "holm",
                        i_am_cheating = FALSE) {

  # Processing
  # -------------------
  if (method == "bayes" | method == "bayesian") {
    return(bayes_cor(df, df2, reorder = TRUE))
  }


  # N samples
  n <- nrow(df)

  # Remove non numeric
  df <- purrr::keep(df, is.numeric)
  if (is.null(df2) == FALSE) {
    df2 <- purrr::keep(df2, is.numeric)
  }

  # P-fishing prevention
  if (ncol(df) > 10 && adjust == "none" && i_am_cheating == FALSE) {
    warning("We've detected that you are running a lot (> 10) of correlation tests without adjusting the p values. To help you in your p-fishing, we've added some interesting variables: You never know, you might find something significant!\nTo deactivate this, change the 'i_am_cheating' argument to TRUE.")
    df_complete <- dplyr::mutate_all(df, dplyr::funs_("replace(., is.na(.), 0)"))
    df$Local_Air_Density <- svd(df_complete)$u[, 1]
    df$Reincarnation_Cycle <- runif(nrow(df), max = 100)
    df$Communism_Level <- -1 * svd(df_complete)$u[, 2]
    df$Alien_Mothership_Distance <- rnorm(nrow(df), mean = 50000, sd = 5000)
    df$Schopenhauers_Optimism <- svd(df_complete)$u[, 3]
    df$Hulks_Power <- runif(nrow(df), max = 10)
  }



  # Compute r coefficients
  if (type == "full") {
    corr <- psych::corr.test(df, y = df2, use = "pairwise", method = method, adjust = "none")
    r <- corr$r
    p <- corr$p
    t <- corr$t
    ci <- corr$ci
    ci.adj <- corr$ci.adj

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
      corr <- qgraph::cor_auto(df, forcePD = FALSE)
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
  if (is.null(p) == FALSE) {
    if (adjust != "none") {
      if ((type == "full" & is.null(df2) == FALSE) | (type == "semi")) {
        p[,] <- p.adjust(p, method = adjust)
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
  if (is.null(p) == FALSE) {
    stars <- ifelse(p < .001, "***",
      ifelse(p < .01, "** ",
        ifelse(p < .05, "* ", " ")
      )
    )
  } else {
    stars <- ""
  }


  # build a new correlation matrix with significance stars
  table <- matrix(paste0(round(r, 2), stars), ncol = ncol(r))


  # Format
  rownames(table) <- colnames(df)
  if (isSymmetric(r)) {
    diag(table) <- paste0(diag(round(r, 2)), " ")
    colnames(table) <- colnames(df)
    table[upper.tri(table, diag = TRUE)] <- "" # remove upper triangle
    table <- as.data.frame(table)
    # remove last column and return the matrix (which is now a data frame)
    summary <- cbind(table[seq_len(length(table) - 1)])
  } else {
    if (is.null(df2)) {
      colnames(table) <- colnames(df)
    } else {
      if (type == "semi") {
        colnames(table) <- colnames(df)
      } else {
        colnames(table) <- colnames(df2)
      }
    }
    table <- as.data.frame(table)
    summary <- table
  }




  # Text
  # -------------
  sentences <- c()
  for (row in seq_len(nrow(r))) {
    for (col in seq_len(ncol(r))) {
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
        significance <- "significant "
      } else if (is.numeric(val_p) & val_p > .05) {
        significance <- "non significant "
      } else {
        significance <- ""
      }


      sentence <- paste0(
        "   - ",
        var1,
        " / ",
        var2,
        ":   ",
        "Results of the ",
        stringr::str_to_title(method),
        " correlation showed a ",
        significance,
        interpret_r(val_r),
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
    " correlation (p value correction: ",
    adjust,
    "):\n"
  ), sentences)

  text <- sentences




  # Plot
  # -------------
  if (is.null(df2) == FALSE & type == "full") {
    corr <- psych::corr.test(cbind(df, df2), use = "pairwise", method = method, adjust = "none")
    r <- corr$r
    p <- corr$p
    p[lower.tri(p)] <- p.adjust(p[lower.tri(p)], method = adjust, n = choose(nrow(p), 2))
    p[upper.tri(p)] <- p.adjust(p[upper.tri(p)], method = adjust, n = choose(nrow(p), 2))
    # warning("Due to the presence of two dataframes, the plot might be incorrect. Consider with caution.")
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
