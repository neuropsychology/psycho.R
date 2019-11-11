



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
























#' Simulates data for single or multiple regression.
#'
#' Simulates data for single or multiple regression.
#'
#' @param coefs Desired theorethical coefs. Can be a single value or a list.
#' @param sample Desired sample size.
#' @param error The error (standard deviation of gaussian noise).
#'
#' @examples
#' library(psycho)
#'
#' data <- simulate_data_regression(coefs = c(0.1, 0.8), sample = 50, error = 0)
#' fit <- lm(y ~ ., data = data)
#' coef(fit)
#' @details See https://stats.stackexchange.com/questions/59062/multiple-linear-regression-simulation
#'
#' @author TPArrow
#'
#' @export
simulate_data_regression <- function(coefs = 0.5, sample = 100, error = 0) {

  # Prevent error
  coefs[coefs == 0] <- 0.01

  y <- rnorm(sample, 0, 1)

  n_var <- length(coefs)
  X <- scale(matrix(rnorm(sample * (n_var), 0, 1), ncol = n_var))
  X <- cbind(y, X)

  # find the current correlation matrix
  cor_0 <- var(X)

  # cholesky decomposition to get independence
  chol_0 <- solve(chol(cor_0))

  X <- X %*% chol_0

  # create new correlation structure (zeros can be replaced with other r vals)
  coefs_structure <- diag(x = 1, nrow = n_var + 1, ncol = n_var + 1)
  coefs_structure[-1, 1] <- coefs
  coefs_structure[1, -1] <- coefs

  X <- X %*% chol(coefs_structure) * sd(y) + mean(y)
  X <- X[, -1]

  # Add noise
  y <- y + rnorm(sample, 0, error)

  data <- data.frame(X)
  names(data) <- paste0("V", 1:n_var)
  data$y <- as.vector(y)

  return(data)
}



















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

































#' Clean and format formula.
#'
#' Clean and format formula.
#'
#' @param formula formula
#' @param ... Arguments passed to or from other methods.
#'
#'
#' @examples
#' library(psycho)
#' library(lme4)
#'
#' fit <- lm(hp ~ wt, data = mtcars)
#'
#' format_formula(fit$call$formula)
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
format_formula <- function(formula) {
  formula <- tryCatch({
    stringr::str_squish(paste(format(eval(formula)), collapse = ""))
  }, error = function(e) {
    formula <- stringr::str_squish(paste(format(formula), collapse = ""))
  })

  return(formula)
}







































# get_graph.lavaan <- function(fit, links = c("Regression", "Correlation", "Loading"), standardize = FALSE, threshold_Coef = NULL, threshold_p = NULL, threshold_MPE = NULL, digits = 2, CI = "default", labels_CI = TRUE, ...) {
#   # https://www.r-bloggers.com/ggplot2-sem-models-with-tidygraph-and-ggraph/
#
#
#   if (labels_CI == TRUE) {
#     if (CI != "default") {
#       results <- analyze(fit, CI = CI, standardize = standardize)
#     } else {
#       results <- analyze(fit, standardize = standardize)
#     }
#   } else {
#     results <- analyze(fit, standardize = standardize)
#   }
#
#   summary <- summary(results)
#   CI <- results$values$CI
#
#   # Check what type of model
#   if (class(fit) %in% c("blavaan")) {
#     summary$Coef <- summary$Median
#     if (is.null(threshold_MPE)) {
#       threshold_MPE <- -1
#     }
#     summary <- summary %>%
#       filter_("MPE >= threshold_MPE")
#   } else if (class(fit) %in% c("lavaan")) {
#     if (is.null(threshold_p)) {
#       threshold_p <- 1.1
#     }
#     summary <- summary %>%
#       filter_("p <= threshold_p")
#   } else {
#     stop(paste("Error in UseMethod('plot_lavaan') : no applicable method for 'plot_lavaan' applied to an object of class", class(fit)))
#   }
#
#   # Deal with thresholds
#   if (is.null(threshold_Coef)) {
#     threshold_Coef <- min(abs(summary$Coef)) - 1
#   }
#
#   # Edge properties
#   edges <- summary %>%
#     mutate_("abs_coef" = "abs(Coef)") %>%
#     filter_(
#       "Type %in% c(links)",
#       "From != To",
#       "abs_coef >= threshold_Coef"
#     ) %>%
#     select(-one_of("abs_coef")) %>%
#     rename_(
#       "to" = "To",
#       "from" = "From"
#     )
#
#   # Labels
#   if (labels_CI == TRUE) {
#     edges <- edges %>%
#       mutate_("Label" = 'paste0(insight::format_value(Coef, digits),
#               ", ", CI, "% CI [", insight::format_value(CI_lower, digits),
#               ", ", insight::format_value(CI_higher, digits), "]")')
#   } else {
#     edges <- edges %>%
#       mutate_("Label" = "insight::format_value(Coef, digits)")
#   }
#   edges <- edges %>%
#     mutate_(
#       "Label_Regression" = "ifelse(Type=='Regression', Label, '')",
#       "Label_Correlation" = "ifelse(Type=='Correlation', Label, '')",
#       "Label_Loading" = "ifelse(Type=='Loading', Label, '')"
#     )
#   edges <- edges[colSums(!is.na(edges)) > 0]
#
#   # Identify latent variables for nodes
#   latent_nodes <- edges %>%
#     filter_('Type == "Loading"') %>%
#     distinct_("to") %>%
#     transmute_("Name" = "to", "Latent" = TRUE)
#
#   nodes_list <- unique(c(edges$from, edges$to))
#
#   # Node properties
#   nodes <- summary %>%
#     filter_(
#       "From == To",
#       "From %in% nodes_list"
#     ) %>%
#     mutate_("Name" = "From") %>%
#     left_join(latent_nodes, by = "Name") %>%
#     mutate_("Latent" = "if_else(is.na(Latent), FALSE, Latent)") %>%
#     select(one_of(c("Name", "Latent")))
#
#   return(list(nodes = nodes, edges = edges))
# }






# get_graph.fa <- function(fit, threshold_Coef = NULL, digits = 2, ...) {
#   edges <- summary(analyze(fit)) %>%
#     tidyr::gather("To", "Coef", -one_of("N", "Item", "Label")) %>%
#     rename_("From" = "Item") %>%
#     mutate_("Label" = "insight::format_value(Coef, digits)") %>%
#     select(one_of("From", "To", "Coef", "Label"), everything()) %>%
#     dplyr::filter()
#
#   # Deal with thresholds
#   if (is.null(threshold_Coef)) {
#     threshold_Coef <- min(abs(edges$Coef)) - 1
#   }
#
#   edges <- edges %>%
#     filter_("Coef > threshold_Coef")
#
#   nodes <- data.frame("Name" = c(edges$From, edges$To)) %>%
#     distinct_("Name")
#
#   return(list(nodes = nodes, edges = edges))
# }




# get_graph.psychobject_correlation <- function(fit, ...) {
#   vars <- row.names(fit$values$r)
#
#   r <- fit$values$r %>%
#     as.data.frame() %>%
#     tibble::rownames_to_column("from") %>%
#     tidyr::gather("to", "r", vars)
#
#   if ("p" %in% names(fit$values)) {
#     r <- r %>%
#       full_join(
#         fit$values$p %>%
#           as.data.frame() %>%
#           tibble::rownames_to_column("from") %>%
#           tidyr::gather("to", "p", vars),
#         by = c("from", "to")
#       )
#   }
#
#   r <- filter_(r, "!from == to")
#   return(r)
# }









































#' Interpret fit measures of blavaan objects
#'
#' Interpret fit measures of blavaan objects
#'
#' @param indices Vector of strings indicating which indices to report. Only works for bayesian objects for now.
#' @param fit A blavaan model.
#' @param ... Other arguments.
#' @export
interpret_blavaan <- function(fit, indices = c("BIC", "DIC", "WAIC", "LOOIC"), ...) {
  values <- list()

  indices <- lavaan::fitmeasures(fit)


  for (index in names(indices)) {
    values[index] <- indices[index]
  }

  # Summary
  summary <- as.data.frame(indices) %>%
    tibble::rownames_to_column("Index") %>%
    rename_("Value" = "indices") %>%
    mutate_("Index" = "str_to_upper(Index)")

  # Text
  relevant_indices <- summary[summary$Index %in% c("BIC", "DIC", "WAIC", "LOOIC"), ]
  text <- paste0(relevant_indices$Index, " = ", insight::format_value(relevant_indices$Value), collapse = ", ")

  output <- list(text = text, summary = summary, values = values, plot = "Not available yet")
  class(output) <- c("psychobject", "list")
  return(output)
}














