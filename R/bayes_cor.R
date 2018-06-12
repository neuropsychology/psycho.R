#' Performs a Bayesian correlation.
#'
#' Performs a Bayesian correlation.
#'
#' @param x First continuous variable.
#' @param y Second continuous variable.
#' @param CI Credible interval bounds.
#' @param iterations The number of iterations to sample.
#' @param effsize_rules_r Grid for effect size interpretation. See \link[=interpret_r]{interpret_r}.
#' @param effsize_rules_bf Grid for effect size interpretation. See \link[=interpret_bf]{interpret_bf}.
#'
#' @return A psychobject.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' x <- psycho::affective$Concealing
#' y <- psycho::affective$Tolerating
#'
#' bayes_cor.test(x, y)
#' summary(bayes_cor.test(x, y))
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom BayesFactor correlationBF posterior
#' @importFrom stats complete.cases cor.test
#' @import dplyr
#' @export
bayes_cor.test <- function(x, y, CI=90, iterations = 10000, effsize_rules_r="cohen1988", effsize_rules_bf="jeffreys1961") {


  # Varnames ----------------------------------------------------------------


  if (is.null(names(x))) {
    var1 <- deparse(substitute(x))
  } else {
    var1 <- names(x)
    x <- pull(x)
  }

  if (is.null(names(y))) {
    var2 <- deparse(substitute(y))
  } else {
    var2 <- names(y)
    y <- pull(y)
  }

  # Remove missing
  var_x <- x[complete.cases(x, y)]
  var_y <- y[complete.cases(x, y)]

  # Correlation -------------------------------------------------------------

  # Stop if same variable
  if (cor.test(var_x, var_y)$estimate > 0.999) {
    return(1)
  }


  cor <- BayesFactor::correlationBF(var_x, var_y)
  posterior <- as.vector(suppressMessages(BayesFactor::posterior(cor, iterations = iterations, progress = FALSE)))

  values <- list()
  values$posterior <- posterior
  values$bf <- as.vector(cor)[1]
  values$median <- median(posterior)
  values$mad <- mad(posterior)
  values$mean <- mean(posterior)
  values$sd <- sd(posterior)
  values$CI <- hdi(posterior, prob = CI / 100)$text
  values$CI_values <- hdi(posterior, prob = CI / 100)
  values$CI_values <- c(values$CI_values$values$HDImin, values$CI_values$values$HDImax)
  values$MPE <- mpe(posterior)$MPE
  values$MPE_values <- mpe(posterior)$values

  norm <- rnorm_perfect(length(posterior), 0, sd(posterior))
  values$overlap <- overlap(posterior, norm) * 100

  rope_indices <- rope(posterior, bounds = c(-0.1, 0.1), CI = 95, overlap = TRUE)
  values$rope_decision <- rope_indices$rope_decision
  values$rope_probability <- rope_indices$rope_probability
  values$rope_overlap <- rope_indices$rope_overlap


  summary <- data.frame(
    Median = values$median,
    MAD = values$mad,
    CI_lower = values$CI_values[1],
    CI_higher = values$CI_values[2],
    MPE = values$MPE,
    BF = values$bf,
    Overlap = values$overlap,
    Rope = values$rope_decision
  )
  rownames(summary) <- paste0(var1, " / ", var2)

  values$effect_size <- interpret_r_posterior(posterior, rules = effsize_rules_r)
  interpretation_r <- interpret_r(values$median, strength = FALSE, rules = effsize_rules_r)
  interpretation_bf <- interpret_bf(values$bf, rules = effsize_rules_bf)

  text <- paste0(
    "Results of the Bayesian correlation indicate ",
    interpretation_bf,
    " a ",
    interpretation_r,
    " association between ",
    var1,
    " and ",
    var2,
    " (r = ",
    format_digit(values$median),
    ", MAD = ",
    format_digit(values$mad),
    ", ",
    CI,
    "% CI [",
    format_digit(values$CI_values[1], null_treshold = 0.0001),
    ", ",
    format_digit(values$CI_values[2], null_treshold = 0.0001),
    "]). ",
    values$effect_size$text
  )

  plot <- "Not available."

  output <- list(text = text, plot = plot, summary = summary, values = values)
  class(output) <- c("psychobject", "list")

  return(output)
}





#' Bayesian Correlation Matrix.
#'
#' Bayesian Correlation Matrix.
#'
#' @param df The dataframe.
#' @param df2 Optional dataframe to correlate with the first one.
#' @param reorder Reorder matrix by correlation strength. Only for square matrices.
#'
#' @return A list of dataframes
#'
#' @examples
#' \dontrun{
#' library(psycho)
#'
#' df <- psycho::affective
#' cor <- bayes_cor(df)
#' summary(cor)
#' print(cor)
#' plot(cor)
#'
#' df <- select(psycho::affective, Adjusting, Tolerating)
#' df2 <- select(psycho::affective, -Adjusting, -Tolerating)
#' cor <- bayes_cor(df, df2)
#' summary(cor)
#' print(cor)
#' plot(cor)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#' @export
bayes_cor <- function(df, df2=NULL, reorder=TRUE) {
  df <- purrr::keep(df, is.numeric)

  if (!is.null(df2)) {
    df2 <- purrr::keep(df2, is.numeric)
    combinations <- expand.grid(names(df), names(df2))
    df <- cbind(df, df2)
  } else {
    combinations <- expand.grid(names(df), names(df))
  }

  size_row <- length(unique(combinations$Var1))
  size_col <- length(unique(combinations$Var2))
  dimnames <- list(
    unique(combinations$Var1),
    unique(combinations$Var2)
  )

  r <- matrix(0, nrow = size_row, ncol = size_col, dimnames = dimnames)
  mpe <- matrix(0, nrow = size_row, ncol = size_col, dimnames = dimnames)
  bf <- matrix(0, nrow = size_row, ncol = size_col, dimnames = dimnames)
  ci <- matrix(0, nrow = size_row, ncol = size_col, dimnames = dimnames)
  text <- matrix("", nrow = size_row, ncol = size_col, dimnames = dimnames)

  counter <- 0
  for (j in seq_len(size_col)) {
    for (i in seq_len(size_row)) {
      counter <- counter + 1

      x <- df[[as.character(combinations$Var1[counter])]]
      y <- df[[as.character(combinations$Var2[counter])]]
      result <- bayes_cor.test(x, y)

      if (!is.psychobject(result)) {
        text[i, j] <- ""
        r[i, j] <- 1
        mpe[i, j] <- 100
        bf[i, j] <- Inf
        ci[i, j] <- "100% CI [1, 1]"
      } else {
        text[i, j] <- paste0(
          "   - ",
          names(df)[j],
          " / ",
          names(df)[i],
          ":   ",
          result$text
        )
        text[i, j] <- stringr::str_remove(text[i, j], "between x and y ")
        r[i, j] <- result$values$median
        mpe[i, j] <- result$values$MPE
        bf[i, j] <- result$values$bf
        ci[i, j] <- result$values$CI
      }
    }
  }


  # Reorder
  if (is.null(df2) & reorder == TRUE) {
    r <- reorder_matrix(r, r)
    mpe <- reorder_matrix(mpe, r)
    bf <- reorder_matrix(bf, r)
    ci <- reorder_matrix(ci, r)
    text <- reorder_matrix(text, r)
  }


  stars <- ifelse(bf > 30, "***",
    ifelse(bf > 10, "**",
      ifelse(bf > 3, "*", "")
    )
  )



  summary <- round(r, 2)
  summary <- matrix(paste(summary, stars, sep = ""), ncol = ncol(r), dimnames = dimnames(r))

  if (is.null(df2)) {
    summary[upper.tri(summary, diag = TRUE)] <- "" # remove upper triangle
    summary <- summary[-1, -ncol(summary)] # Remove first row and last column

    text[upper.tri(text, diag = TRUE)] <- "" # remove upper triangle
    text <- text[-1, -ncol(text)] # Remove first row and last column
  }

  summary <- as.data.frame(summary)
  text <- as.vector(text)
  text <- text[!text == ""]


  # Values
  values <- list(
    r = r,
    mpe = mpe,
    bf = bf,
    ci = ci,
    stars = stars
  )

  # Plot
  plot <- round(r, 2) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Var1") %>%
    gather_("Var2", "Correlation", as.character(unique(combinations$Var2))) %>%
    ggplot(aes_string(x = "Var2", y = "Var1", fill = "Correlation", label = "Correlation")) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "#2196F3", high = "#E91E63", mid = "white",
      midpoint = 0, limit = c(-1, 1)
    ) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      legend.position = "none"
    ) +
    coord_fixed() +
    geom_text(color = "black")


  # Output
  # -------------
  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}



#' Reorder square matrix.
#'
#' Reorder square matrix.
#'
#' @param mat A square matrix.
#' @param dmat A square matrix with values to use as distance.
#'
#' @examples
#' library(psycho)
#'
#' r <- correlation(iris)
#' r <- r$values$r
#' r <- reorder_matrix(r)
#'
#' @importFrom stats as.dist hclust
#' @export
reorder_matrix <- function(mat, dmat=NULL) {
  if (is.null(dmat)) {
    dmat <- mat
  }

  if (ncol(mat) != nrow(mat) | ncol(dmat) != nrow(dmat)) {
    warning("Matrix must be squared.")
    return(mat)
  }

  dmat <- as.dist((1 - dmat) / 2, diag = TRUE, upper = TRUE)
  hc <- hclust(dmat)
  mat <- mat[hc$order, hc$order]
  return(mat)
}
