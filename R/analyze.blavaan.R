#' Analyze blavaan (SEM or CFA) objects.
#'
#' Analyze blavaan (SEM or CFA) objects.
#'
#' @param x lavaan object.
#' @param CI Credible interval level.
#' @param standardize Compute standardized coefs.
#' @param ... Arguments passed to or from other methods.
#'
#' @return output
#'
#' @examples
#' library(psycho)
#' library(lavaan)
#' 
#' model <- " visual  =~ x1 + x2 + x3\ntextual =~ x4 + x5 + x6\nspeed   =~ x7 + x8 + x9 "
#' x <- lavaan::cfa(model, data = HolzingerSwineford1939)
#' 
#' rez <- analyze(x)
#' print(rez)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @seealso
#' https://www.researchgate.net/post/Whats_the_standard_of_fit_indices_in_SEM
#'
#' @importFrom lavaan parameterEstimates fitmeasures
#'
#' @export
analyze.blavaan <- function(x, CI = 90, standardize = FALSE, ...) {
  fit <- x


  # Processing
  # -------------
  values <- list()
  values$CI <- CI

  # Fit measures
  values$Fit_Measures <- interpret_lavaan(fit)


  # Text
  # -------------
  computations <- .get_info_computations(fit)
  fitmeasures <- values$Fit_Measures$text
  text <- paste0(
    "A Bayesian model was fitted (",
    computations,
    "). The fit indices are as following: ",
    fitmeasures
  )

  # Summary
  # -------------
  summary <- .summary_blavaan(fit, CI = CI, standardize = standardize)

  # Plot
  # -------------
  plot <- "Use `get_graph` in association with ggraph."

  output <- list(text = values$Fit_Measures$text, plot = plot, summary = summary, values = values)

  class(output) <- c("psychobject", "list")
  return(output)
}






#' @keywords internal
.get_info_computations <- function(fit) {
  chains <- blavaan::blavInspect(fit, "n.chains")
  sample <- fit@external$sample
  warmup <- fit@external$burnin
  text <- paste0(
    chains,
    " chains, each with iter = ",
    sample,
    "; warmup = ",
    warmup
  )
  return(text)
}




#' @keywords internal
.process_blavaan <- function(fit, standardize = FALSE, CI = 90) {
  # Get relevant rows
  PE <- parameterEstimates(fit,
    se = FALSE, ci = FALSE, remove.eq = FALSE, remove.system.eq = TRUE,
    remove.ineq = FALSE, remove.def = FALSE,
    add.attributes = TRUE
  )
  if (!("group" %in% names(PE))) PE$group <- 1
  newpt <- fit@ParTable
  pte2 <- which(newpt$free > 0)
  relevant_rows <- match(
    with(newpt, paste(lhs[pte2], op[pte2], rhs[pte2], group[pte2], sep = "")),
    paste(PE$lhs, PE$op, PE$rhs, PE$group, sep = "")
  )

  # Priors
  priors <- rep(NA, nrow(PE))
  priors[relevant_rows] <- newpt$prior[pte2]
  priors[is.na(PE$prior)] <- ""




  # Posterior
  if (standardize == FALSE) {
    posteriors <- blavaan::blavInspect(fit, "draws") %>%
      as.matrix() %>%
      as.data.frame()
    names(posteriors) <- names(lavaan::coef(fit))
  } else {
    posteriors <- blavaan::standardizedposterior(fit) %>%
      as.data.frame()
  }



  # Effects
  MPE <- c()
  Median <- c()
  MAD <- c()
  Effect <- c()
  CI_lower <- c()
  CI_higher <- c()
  for (effect in names(posteriors)) {
    posterior <- posteriors[[effect]]
    Effect <- c(Effect, effect)
    MPE <- c(MPE, mpe(posterior)$MPE)
    Median <- c(Median, median(posterior))
    MAD <- c(MAD, mad(posterior))

    CI_values <- HDI(posterior, prob = CI / 100)
    CI_lower <- c(CI_lower, CI_values$values$HDImin)
    CI_higher <- c(CI_higher, CI_values$values$HDImax)
  }

  if (standardize == FALSE) {
    Effects <- rep(NA, nrow(PE))
    Effects[relevant_rows] <- Effect
    MPEs <- rep(NA, nrow(PE))
    MPEs[relevant_rows] <- MPE
    Medians <- rep(NA, nrow(PE))
    Medians[relevant_rows] <- Median
    MADs <- rep(NA, nrow(PE))
    MADs[relevant_rows] <- MAD
    CI_lowers <- rep(NA, nrow(PE))
    CI_lowers[relevant_rows] <- CI_lower
    CI_highers <- rep(NA, nrow(PE))
    CI_highers[relevant_rows] <- CI_higher
  } else {
    Effects <- Effect
    MPEs <- MPE
    Medians <- Median
    MADs <- MAD
    CI_lowers <- CI_lower
    CI_highers <- CI_higher
  }

  data <- data.frame(
    "Effect" = Effects,
    "Median" = Medians,
    "MAD" = MADs,
    "MPE" = MPEs,
    "CI_lower" = CI_lowers,
    "CI_higher" = CI_highers,
    "Prior" = priors
  )

  return(data)
}



#' @keywords internal
.summary_blavaan <- function(fit, CI = 90, standardize = FALSE) {
  solution <- lavaan::parameterEstimates(fit, se = TRUE, ci = TRUE, standardized = FALSE, level = CI / 100)

  solution <- solution %>%
    rename(
      "From" = "rhs",
      "To" = "lhs",
      "Operator" = "op",
      "Coef" = "est",
      "SE" = "se",
      "CI_lower" = "ci.lower",
      "CI_higher" = "ci.upper"
    ) %>%
    mutate(Type = dplyr::case_when(
      Operator == "=~" ~ "Loading",
      Operator == "~" ~ "Regression",
      Operator == "~~" ~ "Correlation",
      TRUE ~ NA_character_
    )) %>%
    select(one_of(c("To", "Operator", "From", "Type"))) %>%
    mutate_("Effect" = "as.character(paste0(To, Operator, From))") %>%
    full_join(.process_blavaan(fit, CI = CI, standardize = standardize) %>%
      mutate_("Effect" = "as.character(Effect)"), by = "Effect") %>%
    select_("-Effect") %>%
    mutate_(
      "Median" = "replace_na(Median, 1)",
      "MAD" = "replace_na(MAD, 0)",
      "MPE" = "replace_na(MPE, 100)"
    ) %>%
    select(one_of(c("From", "Operator", "To", "Median", "MAD", "CI_lower", "CI_higher", "MPE", "Prior", "Type"))) %>%
    dplyr::filter_("Operator != '~1'")


  return(solution)
}
