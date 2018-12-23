#' Find Optimal Factor Number.
#'
#' Find optimal components number using maximum method aggreement.
#'
#' @param df A dataframe or correlation matrix
#' @param rotate What rotation to use c("none", "varimax", "oblimin","promax")
#' @param fm Factoring method: "pa" for Principal Axis Factor Analysis,
#' "minres" (default) for minimum residual (OLS) factoring, "mle" for
#' Maximum Likelihood FA and "pc" for Principal Components
#' @param n If correlation matrix is passed, the sample size.
#'
#' @return output
#'
#' @examples
#' df <- dplyr::select_if(attitude, is.numeric)
#' results <- psycho::n_factors(df)
#' 
#' summary(results)
#' plot(results)
#' 
#' # See details on methods
#' psycho::values(results)$methods
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom qgraph cor_auto
#' @importFrom psych VSS
#' @importFrom MASS mvrnorm
#' @importFrom MASS ginv
#' @importFrom nFactors moreStats
#' @importFrom nFactors nScree
#' @importFrom stats cov
#' @importFrom stats dnorm
#' @importFrom stats qnorm
#' @export
n_factors <- function(df, rotate = "varimax", fm = "minres", n = NULL) {

  # Copy the parallel function from nFactors to correct the use of mvrnorm
  parallel <- function(subject = 100, var = 10, rep = 100, cent = 0.05,
                         quantile = cent, model = "components",
                         sd = diag(1, var), ...) {
    r <- subject
    c <- var
    y <- matrix(c(1:r * c), nrow = r, ncol = c)
    evpea <- NULL
    for (k in c(1:rep)) {
      y <- MASS::mvrnorm(n = r, mu = rep(0, var), Sigma = sd, empirical = FALSE)
      corY <- cov(y, ...)
      if (model == "components") {
        diag(corY) <- diag(sd)
      }
      if (model == "factors") {
        corY <- corY - MASS::ginv(diag(diag(MASS::ginv(corY))))
      }
      evpea <- rbind(evpea, eigen(corY)[[1]])
    }
    SEcentile <- function(sd, n = 100, p = 0.95) {
      return(sd / sqrt(n) * sqrt(p * (1 - p)) / dnorm(qnorm(p)))
    }
    mevpea <- sapply(as.data.frame(evpea), mean)
    sevpea <- sapply(as.data.frame(evpea), sd)
    qevpea <- nFactors::moreStats(evpea, quantile = quantile)[3, ]
    sqevpea <- sevpea
    sqevpea <- sapply(
      as.data.frame(sqevpea), SEcentile,
      n = rep,
      p = cent
    )
    result <- list(
      eigen = data.frame(
        mevpea, sevpea, qevpea,
        sqevpea
      ),
      subject = r,
      variables = c,
      centile = cent
    )
    class(result) <- "parallel"
    return(result)
  }

  # Detect if df us a correlation matrix
  if (length(setdiff(names(df), rownames(df))) != 0) {
    cor <- qgraph::cor_auto(df, forcePD = FALSE)
    n <- nrow(df)
  } else {
    if (is.null(n)) {
      stop("A correlation matrix was passed. You must provided the sample size (n).")
    }
    cor <- df
  }


  ap <- parallel(subject = n, var = ncol(cor))
  nS <- nFactors::nScree(x = eigen(cor)$values, aparallel = ap$eigen$qevpea)

  # Eigeinvalues data
  eigenvalues <- nS$Analysis %>%
    dplyr::select_(
      "Eigenvalues",
      "Exp.Variance" = "Prop",
      "Cum.Variance" = "Cumu"
    ) %>%
    mutate_("n.Factors" = ~ seq_len(nrow(nS$Analysis)))





  # Processing
  # -------------------
  results <- data.frame(
    Method = c(
      "Optimal Coordinates",
      "Acceleration Factor",
      "Parallel Analysis",
      "Eigenvalues (Kaiser Criterion)"
    ),
    n_optimal = as.numeric(nS$Components[1, ])
  )

  # EGA Method
  # Doesn't really work for now :(
  # ega <- EGA::EGA(cor, plot.EGA = F, matrix=TRUE, n = n)
  # ega <- EGA::bootEGA(df, n = 1000)

  # VSS
  vss <- psych::VSS(
    cor,
    n.obs = n,
    rotate = rotate,
    fm = fm, plot = F
  ) # fm can be "pa", "pc", "minres", "mle"
  stats <- vss$vss.stats
  stats$map <- vss$map
  stats$n_factors <- seq_len(nrow(stats))

  # map
  if (length(stats$map[!is.na(stats$map)]) > 0) {
    min <- min(stats$map[!is.na(stats$map)])
    opt <- stats[stats$map == min, ]$n_factors[!is.na(stats[stats$map == min, ]$n_factors)]
    results <- rbind(
      results,
      data.frame(
        Method = c("Velicer MAP"),
        n_optimal = c(opt)
      )
    )
  }
  # bic
  if (length(stats$BIC[!is.na(stats$BIC)]) > 0) {
    min <- min(stats$BIC[!is.na(stats$BIC)])
    opt <- stats[stats$BIC == min, ]$n_factors[!is.na(stats[stats$BIC == min, ]$n_factors)]
    results <- rbind(
      results,
      data.frame(
        Method = c("BIC"),
        n_optimal = c(opt)
      )
    )
  }
  # sabic
  if (length(stats$SABIC[!is.na(stats$SABIC)]) > 0) {
    min <- min(stats$SABIC[!is.na(stats$SABIC)])
    opt <- stats[stats$SABIC == min, ]$n_factors[!is.na(stats[stats$SABIC == min, ]$n_factors)]
    results <- rbind(
      results,
      data.frame(
        Method = c("Sample Size Adjusted BIC"),
        n_optimal = c(opt)
      )
    )
  }


  cfits <- vss[grep("cfit", names(vss))]
  for (name in names(cfits)) {
    cfit <- cfits[[name]]

    cfit <- data.frame(cfit = cfit, n_factors = seq_len(length(cfit)))

    result3 <- data.frame(
      Method = c(gsub("cfit.", "VSS Complexity ", name)),
      n_optimal = c(na.omit(cfit[cfit$cfit == max(cfit$cfit, na.rm = TRUE), ])$n_factors)
    )

    results <- rbind(results, result3)
  }


  eigenvalues <- results %>%
    group_by_("n_optimal") %>%
    summarise_("n_method" = ~ n()) %>%
    mutate_("n_optimal" = ~ factor(n_optimal, levels = seq_len(nrow(eigenvalues)))) %>%
    complete_("n_optimal", fill = list(n_method = 0)) %>%
    arrange_("n_optimal") %>%
    rename_(
      "n.Factors" = "n_optimal",
      "n.Methods" = "n_method"
    ) %>%
    mutate_("n.Factors" = ~ as.integer(n.Factors)) %>%
    left_join(eigenvalues, by = "n.Factors") %>%
    select_("-Exp.Variance")


  # Summary
  # -------------
  summary <- eigenvalues

  # Values
  # -------------

  best_n_df <- filter_(summary, "n.Methods == max(n.Methods)")
  best_n <- best_n_df$n.Factors

  best_n_methods <- list()
  for (i in as.list(best_n)) {
    methods_list <- results[results$n_optimal %in% as.list(i), ]
    methods_list <- as.character(methods_list$Method)
    best_n_methods[[paste0("n_", i)]] <- paste(methods_list, collapse = ", ")
  }



  values <- list(summary = summary, methods = results, best_n_df = best_n)



  # Text
  # -------------
  # Deal with equality
  if (length(best_n) > 1) {
    best_n <- head(best_n, length(best_n) - 1) %>%
      paste(collapse = ", ") %>%
      paste(best_n[length(best_n)], sep = " and ")
    factor_text <- " factors "
    n_methods <- unique(best_n_df$n.Methods)
    best_n_methods <- paste0(paste(best_n_methods, collapse = "; "), "; respectively")
  } else {
    n_methods <- best_n_df$n.Methods
    # Plural
    if (best_n == 1) {
      factor_text <- " factor "
    } else {
      factor_text <- " factors "
    }
  }



  text <- paste0(
    "The choice of ",
    best_n,
    factor_text,
    "is supported by ",
    n_methods,
    " (out of ",
    round(nrow(results)),
    "; ",
    round(n_methods / nrow(results) * 100, 2),
    "%) methods (",
    best_n_methods,
    ")."
  )


  # Plot
  # -------------
  plot_data <- summary
  plot_data$n.Methods.Ratio <- plot_data$n.Methods / sum(plot_data$n.Methods)
  plot_data$n.Methods.Ratio <- plot_data$n.Methods.Ratio * (1 / max(plot_data$n.Methods.Ratio))
  plot_data$area <- plot_data$n.Methods.Ratio / (max(plot_data$n.Methods.Ratio) / max(plot_data$Eigenvalues))
  plot_data$var <- plot_data$Cum.Variance / (max(plot_data$Cum.Variance) / max(plot_data$Eigenvalues))

  plot <- plot_data %>%
    ggplot(aes_string(x = "n.Factors", y = "Eigenvalues")) +
    geom_area(
      aes_string(y = "area"),
      fill = "#FFC107",
      alpha = 0.5
    ) +
    geom_line(
      colour = "#E91E63",
      size = 1
    ) +
    geom_hline(yintercept = 1, linetype = "dashed", colour = "#607D8B") +
    geom_line(
      aes_string(y = "var"),
      colour = "#2196F3",
      size = 1
    ) +
    scale_y_continuous(sec.axis = sec_axis(
      trans = ~ . * (max(plot_data$Cum.Variance) / max(plot_data$Eigenvalues)),
      name = "Cumulative Variance\n"
    )) +
    ylab("Eigenvalues\n") +
    xlab("\nNumber of Factors") +
    theme_minimal()

  # Output
  # -------------
  output <- list(text = text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}
