#' Returns the best combination of predictors based on LOO cross-validation indices.
#'
#' Returns the best combination of predictors based on LOO cross-validation indices.
#'
#' @param fit A stanreg object.
#' @param interaction Include interaction term.
#' @param fixed Additional formula part to add at the beginning of
#' each formula
#' @param K For kfold, the number of subsets of equal (if possible) size into
#' which the data will be randomly partitioned for performing K-fold
#' cross-validation. The model is refit K times, each time leaving out one of
#' the K subsets. If K is equal to the total number of observations in the data
#' then K-fold cross-validation is equivalent to exact leave-one-out
#' cross-validation.
#' @param k_treshold Threshold for flagging estimates of the Pareto shape
#' parameters k estimated by loo.
#' @param ... Arguments passed to or from other methods.
#'
#' @return list containing all combinations.
#'
#' @examples
#' \dontrun{
#' library(psycho)
#' library(rstanarm)
#'
#' data <- standardize(attitude)
#' fit <- rstanarm::stan_glm(rating ~ advance + privileges, data=data)
#'
#' best <- find_best_model(fit)
#' best_formula <- best$formula
#' best$table
#'
#' # To deactivate Kfold evaluation
#' best <- find_best_model(fit, K=0)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom rstanarm loo kfold bayes_R2
#' @importFrom stats update median
#' @import dplyr
#'
#' @method find_best_model stanreg
#' @export
find_best_model.stanreg <- function(fit, interaction=TRUE, fixed=NULL, K=10, k_treshold=NULL, ...) {

  # Extract infos
  combinations <- find_combinations(fit$formula, interaction = interaction, fixed = fixed)

  # Compute fitting indices
  loos <- list()
  kfolds <- list()
  complexities <- list()
  R2s <- list()
  for (i in seq_len(length(combinations))) {
    print(paste0(i, "/", length(combinations)))

    formula <- combinations[i]
    newfit <- update(fit, formula = formula, verbose = FALSE)
    R2s[[formula]] <- median(rstanarm::bayes_R2(newfit))
    
    
    if (!is.null(k_treshold)) {
      loo <- rstanarm::loo(newfit, k_treshold = k_treshold)
    } else {
      loo <- rstanarm::loo(newfit)
    }
    
    
    complexities[[formula]] <- length(newfit$coefficients)
    loos[[formula]] <- loo
    if (K > 1) {
      kfold <- rstanarm::kfold(newfit, K = K)
    } else {
      kfold <- list(elpd_kfold = 0, se_elpd_kfold = 0)
    }
    kfolds[[formula]] <- kfold
  }

  # Model comparison
  comparison <- data.frame()
  for (formula in names(loos)) {
    loo <- loos[[formula]]
    kfold <- kfolds[[formula]]
    complexity <- complexities[[formula]]
    Estimates <- loo[["estimates"]]
    model <- data.frame(
      formula = formula,
      complexity = complexity - 1,
      R2 = R2s[[formula]],
      looic = Estimates["looic","Estimate"],
      looic_se = Estimates["looic","SE"],
      elpd_loo = Estimates["elpd_loo","Estimate"],
      elpd_loo_se = Estimates["elpd_loo","SE"],
      p_loo = Estimates["p_loo","Estimate"],
      p_loo_se = Estimates["p_loo","SE"],
      elpd_kfold = Estimates["p_loo","Estimate"],
      elpd_kfold_se = Estimates["p_loo","SE"]
    )
    comparison <- rbind(comparison, model)
  }

  # Format
  comparison <- comparison %>%
    dplyr::mutate_(
      "looic_d" = "looic - min(looic)",
      "elpd_loo_d" = "elpd_loo - max(elpd_loo)",
      "elpd_kfold_d" = "elpd_kfold - max(elpd_kfold)"
    )

  # Best model by criterion
  best_looic <- dplyr::arrange_(comparison, "looic") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_looic <- as.character(best_looic[[1]])

  best_elpd_loo <- dplyr::arrange_(comparison, "desc(elpd_loo)") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_elpd_loo <- as.character(best_elpd_loo[[1]])

  if (K > 1) {
    best_elpd_kfold <- dplyr::arrange_(comparison, "desc(elpd_kfold)") %>%
      dplyr::select_("formula") %>%
      head(1)
    best_elpd_kfold <- as.character(best_elpd_kfold[[1]])
  } else {
    best_elpd_kfold <- NA
  }

  by_criterion <- data.frame(formula = c(best_looic, best_elpd_loo, best_elpd_kfold), criterion = c("looic", "elpd_loo", "elpd_kfold"))

  # Best formula
  best <- table(by_criterion$formula)
  best <- names(best[which.max(best)])

  best <- list(formula = best, by_criterion = by_criterion, table = comparison)
  return(best)
}
