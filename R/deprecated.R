#' Remove empty columns.
#'
#' Removes all columns containing ony NaNs.
#'
#' @param df Dataframe.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
remove_empty_cols <- function(df) {
  df <- df[, colSums(is.na(df)) < nrow(df)]
  return(df)
}




#' Creates or tests for objects of mode "psychobject".
#'
#' @param x an arbitrary R object.
#'
#' @export
is.psychobject <- function(x) inherits(x, "psychobject")














# analyze.blavaan <- function(x, CI = 90, standardize = FALSE, ...) {
#   fit <- x
#
#
#   # Processing
#   # -------------
#   values <- list()
#   values$CI <- CI
#
#   # Fit measures
#   values$Fit_Measures <- interpret_blavaan(fit)
#
#
#   # Text
#   # -------------
#   computations <- .get_info_computations(fit)
#   fitmeasures <- values$Fit_Measures$text
#   text <- paste0(
#     "A Bayesian model was fitted (",
#     computations,
#     "). The fit indices are as following: ",
#     fitmeasures
#   )
#
#   # Summary
#   # -------------
#   summary <- .summary_blavaan(fit, CI = CI, standardize = standardize)
#
#   # Plot
#   # -------------
#   plot <- "Use `get_graph` in association with ggraph."
#
#   output <- list(text = values$Fit_Measures$text, plot = plot, summary = summary, values = values)
#
#   class(output) <- c("psychobject", "list")
#   return(output)
# }




# .get_info_computations <- function(fit) {
#   chains <- blavaan::blavInspect(fit, "n.chains")
#   sample <- fit@external$sample
#   warmup <- fit@external$burnin
#   text <- paste0(
#     chains,
#     " chains, each with iter = ",
#     sample,
#     "; warmup = ",
#     warmup
#   )
#   return(text)
# }




# .process_blavaan <- function(fit, standardize = FALSE, CI = 90) {
#   # Get relevant rows
#   PE <- parameterEstimates(fit,
#     se = FALSE, ci = FALSE, remove.eq = FALSE, remove.system.eq = TRUE,
#     remove.ineq = FALSE, remove.def = FALSE,
#     add.attributes = TRUE
#   )
#   if (!("group" %in% names(PE))) PE$group <- 1
#   newpt <- fit@ParTable
#   pte2 <- which(newpt$free > 0)
#   relevant_rows <- match(
#     with(newpt, paste(lhs[pte2], op[pte2], rhs[pte2], group[pte2], sep = "")),
#     paste(PE$lhs, PE$op, PE$rhs, PE$group, sep = "")
#   )
#
#   # Priors
#   priors <- rep(NA, nrow(PE))
#   priors[relevant_rows] <- newpt$prior[pte2]
#   priors[is.na(PE$prior)] <- ""
#
#
#
#
#   # Posterior
#   if (standardize == FALSE) {
#     posteriors <- blavaan::blavInspect(fit, "draws") %>%
#       as.matrix() %>%
#       as.data.frame()
#     names(posteriors) <- names(lavaan::coef(fit))
#   } else {
#     posteriors <- blavaan::standardizedposterior(fit) %>%
#       as.data.frame()
#   }
#
#
#
#   # Effects
#   MPE <- c()
#   Median <- c()
#   MAD <- c()
#   Effect <- c()
#   CI_lower <- c()
#   CI_higher <- c()
#   for (effect in names(posteriors)) {
#     posterior <- posteriors[[effect]]
#     Effect <- c(Effect, effect)
#     MPE <- c(MPE, mpe(posterior)$MPE)
#     Median <- c(Median, median(posterior))
#     MAD <- c(MAD, mad(posterior))
#
#     CI_values <- HDI(posterior, prob = CI / 100)
#     CI_lower <- c(CI_lower, CI_values$values$HDImin)
#     CI_higher <- c(CI_higher, CI_values$values$HDImax)
#   }
#
#   if (standardize == FALSE) {
#     Effects <- rep(NA, nrow(PE))
#     Effects[relevant_rows] <- Effect
#     MPEs <- rep(NA, nrow(PE))
#     MPEs[relevant_rows] <- MPE
#     Medians <- rep(NA, nrow(PE))
#     Medians[relevant_rows] <- Median
#     MADs <- rep(NA, nrow(PE))
#     MADs[relevant_rows] <- MAD
#     CI_lowers <- rep(NA, nrow(PE))
#     CI_lowers[relevant_rows] <- CI_lower
#     CI_highers <- rep(NA, nrow(PE))
#     CI_highers[relevant_rows] <- CI_higher
#   } else {
#     Effects <- Effect
#     MPEs <- MPE
#     Medians <- Median
#     MADs <- MAD
#     CI_lowers <- CI_lower
#     CI_highers <- CI_higher
#   }
#
#   data <- data.frame(
#     "Effect" = Effects,
#     "Median" = Medians,
#     "MAD" = MADs,
#     "MPE" = MPEs,
#     "CI_lower" = CI_lowers,
#     "CI_higher" = CI_highers,
#     "Prior" = priors
#   )
#
#   return(data)
# }



# .summary_blavaan <- function(fit, CI = 90, standardize = FALSE) {
#   solution <- lavaan::parameterEstimates(fit, se = TRUE, ci = TRUE, standardized = FALSE, level = CI / 100)
#
#   solution <- solution %>%
#     rename(
#       "From" = "rhs",
#       "To" = "lhs",
#       "Operator" = "op",
#       "Coef" = "est",
#       "SE" = "se",
#       "CI_lower" = "ci.lower",
#       "CI_higher" = "ci.upper"
#     ) %>%
#     mutate(Type = dplyr::case_when(
#       Operator == "=~" ~ "Loading",
#       Operator == "~" ~ "Regression",
#       Operator == "~~" ~ "Correlation",
#       TRUE ~ NA_character_
#     )) %>%
#     select(one_of(c("To", "Operator", "From", "Type"))) %>%
#     mutate_("Effect" = "as.character(paste0(To, Operator, From))") %>%
#     full_join(.process_blavaan(fit, CI = CI, standardize = standardize) %>%
#       mutate_("Effect" = "as.character(Effect)"), by = "Effect") %>%
#     select_("-Effect") %>%
#     mutate_(
#       "Median" = "replace_na(Median, 1)",
#       "MAD" = "replace_na(MAD, 0)",
#       "MPE" = "replace_na(MPE, 100)"
#     ) %>%
#     select(one_of(c("From", "Operator", "To", "Median", "MAD", "CI_lower", "CI_higher", "MPE", "Prior", "Type"))) %>%
#     dplyr::filter_("Operator != '~1'")
#
#
#   return(solution)
# }







# interpret_blavaan <- function(fit, indices = c("BIC", "DIC", "WAIC", "LOOIC"), ...) {
#   values <- list()
#
#   indices <- lavaan::fitmeasures(fit)
#
#
#   for (index in names(indices)) {
#     values[index] <- indices[index]
#   }
#
#   # Summary
#   summary <- as.data.frame(indices) %>%
#     tibble::rownames_to_column("Index") %>%
#     rename_("Value" = "indices") %>%
#     mutate_("Index" = "str_to_upper(Index)")
#
#   # Text
#   relevant_indices <- summary[summary$Index %in% c("BIC", "DIC", "WAIC", "LOOIC"), ]
#   text <- paste0(relevant_indices$Index, " = ", insight::format_value(relevant_indices$Value), collapse = ", ")
#
#   output <- list(text = text, summary = summary, values = values, plot = "Not available yet")
#   class(output) <- c("psychobject", "list")
#   return(output)
# }






























#' Generate all combinations.
#'
#' Generate all combinations.
#'
#' @param object Object
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
find_combinations <- function(object, ...) {
  UseMethod("find_combinations")
}


#' Generate all combinations of predictors of a formula.
#'
#' Generate all combinations of predictors of a formula.
#'
#' @param object Formula.
#' @param interaction Include interaction term.
#' @param fixed Additional formula part to add at the beginning of
#' each combination.
#' @param ... Arguments passed to or from other methods.
#'
#' @return list containing all combinations.
#'
#' @examples
#' library(psycho)
#'
#' f <- as.formula("Y ~ A + B + C + D")
#' f <- as.formula("Y ~ A + B + C + D + (1|E)")
#' f <- as.formula("Y ~ A + B + C + D + (1|E) + (1|F)")
#'
#' find_combinations(f)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method find_combinations formula
#' @importFrom utils combn
#' @importFrom stats terms
#' @export
find_combinations.formula <- function(object, interaction = TRUE, fixed = NULL, ...) {

  # Extract infos
  formula <- object
  vars <- attributes(terms(formula))$term.labels
  outcome <- all.vars(formula)[1]
  pred <- vars[!grepl("\\|", vars)]
  if (length(vars[grepl("\\|", vars)]) > 0) {
    random <- paste0(" + (", vars[grepl("\\|", vars)], ")")
  } else {
    random <- ""
  }

  if (is.null(fixed)) {
    fixed <- ""
  } else {
    fixed <- fixed
  }

  # Generate combinations
  n <- length(pred)

  id <- unlist(
    lapply(
      1:n,
      function(i) combn(1:n, i, simplify = FALSE)
    ),
    recursive = FALSE
  )

  combinations <- sapply(id, function(i)
    paste(paste(pred[i], collapse = " + ")))


  # Generate interactions
  if (interaction == TRUE) {
    for (comb in combinations) {
      n_signs <- stringr::str_count(comb, "\\+")
      if (n_signs > 0) {
        new_formula <- comb
        for (i in 1:n_signs) {
          new_formula <- stringr::str_replace(new_formula, "\\+", "*")
          combinations <- c(combinations, new_formula)
        }
      }
    }
  }

  combinations <- paste0(outcome, " ~ ", fixed, combinations, paste0(random, collapse = ""))
  return(combinations)
}


