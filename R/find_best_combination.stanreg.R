#' Returns the best combination of predictors based on LOO cross-validation indices.
#'
#' Returns the best combination of predictors based on LOO cross-validation indices.
#'
#' @param fit A stanreg object.
#' @param interaction Include interaction term.
#' @param K dupa
#' @param k_treshold dupa
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
#' best <- find_best_combination(fit)
#' best_formula <- best$formula
#' best$table
#'
#' # To deactivate Kfold evaluation
#' best <- find_best_combination(fit, K=0)
#' }
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @importFrom rstanarm loo kfold
#' @importFrom stats update
#' @import dplyr
#'
#' @method find_best_combination stanreg
#' @export
find_best_combination.stanreg <- function(fit, interaction=TRUE, K=10, k_treshold=NULL, ...){

  # Extract infos
  f <- fit$formula

  combinations <- find_combinations(f, interaction=interaction)

  # Compute fitting indices
  loos <- list()
  kfolds <- list()
  for(i in 1:length(combinations)){
    print(paste0(i, "/", length(combinations)))

    formula <- combinations[i]
    newfit <- update(fit, formula=formula, verbose = F)
    loo <- rstanarm::loo(newfit)
    loo$complexity <- length(newfit$coefficients)
    loos[[formula]] <- loo
    if(K>1){
      kfold <- rstanarm::kfold(newfit, K=K)
      kfolds[[formula]] <- kfold
    } else{
      kfold <- list(elpd_kfold=0, se_elpd_kfold=0)
    }


  }

  # Model comparison
  comparison <- data.frame()
  for(formula in names(loos)){
    loo <- loos[[formula]]
    kfold <- kfolds[[formula]]
    model <- data.frame(formula = formula,
                      complexity = loo$complexity-1,
                      looic = loo$looic,
                      looic_se = loo$se_looic,
                      elpd_loo = loo$elpd_loo,
                      elpd_loo_se = loo$se_elpd_loo,
                      p_loo = loo$p_loo,
                      p_loo_se = loo$se_p_loo,
                      elpd_kfold = kfold$elpd_kfold,
                      elpd_kfold_se = kfold$se_elpd_kfold)
    comparison <- rbind(comparison, model)
  }

  # Format
  comparison <- comparison %>%
    dplyr::mutate_("looic_d" = "looic - min(looic)",
                  "elpd_loo_d" = "elpd_loo - max(elpd_loo)",
                  "elpd_kfold_d" = "elpd_kfold - max(elpd_kfold)")

  # Best model by criterion
  best_looic <- dplyr::arrange_(comparison, "looic") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_looic <- as.character(best_looic[[1]])

  best_elpd_loo <- dplyr::arrange_(comparison, "desc(elpd_loo)") %>%
    dplyr::select_("formula") %>%
    head(1)
  best_elpd_loo <- as.character(best_elpd_loo[[1]])

  if(K>1){
    best_elpd_kfold <- dplyr::arrange_(comparison, "desc(elpd_kfold)") %>%
      dplyr::select_("formula") %>%
      head(1)
    best_elpd_kfold <- as.character(best_elpd_kfold[[1]])
  } else{
    best_elpd_kfold <- NA
  }

  by_criterion <- data.frame(formula = c(best_looic, best_elpd_loo, best_elpd_kfold), criterion = c("looic", "elpd_loo", "elpd_kfold"))

  # Best formula
  best <- table(by_criterion$formula)
  best <-names(best[which.max(best)])

  best <- list(formula=best, by_criterion=by_criterion, table=comparison)
  return(best)
}

