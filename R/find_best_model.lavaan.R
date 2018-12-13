#' Returns all combinations of lavaan models with their indices of fit.
#'
#' Returns all combinations of lavaan models with their indices of fit.
#'
#' @param fit A lavaan object.
#' @param samples Number of random draws.
#' @param verbose Show progress.
#' @param ... Arguments passed to or from other methods.
#'
#' @return list containing all combinations.
#'
#' @examples
#' library(psycho)
#' library(lavaan)
#'
#' model <- ' visual  =~ x1 + x2 + x3
#'            textual =~ x4 + x5 + x6
#'            speed   =~ x7 + x8 + x9
#'            visual ~ textual
#'            textual ~ speed'
#' fit <- lavaan::sem(model, data=HolzingerSwineford1939)
#'
#' models <- find_best_model.lavaan(fit, latent="visual  =~ x1 + x2 + x3
#'                              textual =~ x4 + x5 + x6
#'                              speed   =~ x7 + x8 + x9")
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import dplyr
#'
#' @method find_best_model stanreg
#' @export
find_best_model.lavaan <- function(fit, latent="", samples=1000, verbose=FALSE, ...){

  update_model <- function(fit, latent, model){
    newfit <- update(fit, paste0(latent, "\n", model))

    indices <- data.frame(Value = fitMeasures(newfit)) %>%
      tibble::rownames_to_column("Index") %>%
      tidyr::spread_("Index", "Value") %>%
      cbind(data.frame(model = model,
                       n_links = nrow(lavaan::lavInspect(fit, "est")$beta)))
    return(indices)
  }

  vars <- row.names(lavaan::lavInspect(fit, "est")$beta)
  # info <- fit@Model

  data <- data.frame()
  for(outcome in vars){
    remaning_vars <- vars[!stringr::str_detect(vars, outcome)]
    combinations <- c()
    for(y in 1:length(remaning_vars)){
      combinations <- c(combinations, combn(remaning_vars, y, simplify=FALSE))
    }
    combinations <- sapply(combinations, paste0, collapse = "+")
    combinations <- paste0(outcome, "~", combinations)
    x <- data.frame(A = combinations)
    names(x) <- c(outcome)
    if(nrow(data) == 0){
      data <- x
    } else{
      data <- cbind(data, x)
    }
  }

  data <- rbind(data, head(data[NA,], 1))
  data[] <- lapply(data, as.character)
  data[is.na(data)] <- ""
  rownames(data) <- NULL

  out <- data.frame()
  for(i in 1:samples){
    if(verbose==TRUE){
      cat(".")
    }
    model <- ""
    for(var in names(data)){
      model <- paste0(model, sample(data[[var]], 1), "\n")
    }

    if(!model %in% out$model){
      out <- tryCatch(
        rbind(out, update_model(fit, latent, model)),
        error=function(e) out,
        warning=function(w) out)
    }
  }
  return(out)
  }



  # vars <- row.names(lavaan::lavInspect(fit, "est")$lambda)
  #
  # possibilities <- list()
  # for(var in vars){
  #   remaning_vars <- paste(vars[!str_detect(vars, var)], collapse = "+")
  #   formula <- as.formula(paste(var, "~", remaning_vars))
  #   combinations <- find_combinations(formula, interaction = FALSE)
  #   possibilities[[var]] <- combinations
  # }
  #
  #
  #
  # get_solution <- function(possibilities, fit, out){
  #   n <- sample(1:length(possibilities), 1)
  #   predictors <- sample(possibilities, n)
  #   model <- ""
  #   for(var in names(predictors)){
  #     model <- paste0(model, sample(predictors[[var]], 1), "\n")
  #     if(model %in% out$model){
  #       next
  #     }
  #   }
  #   newfit <- update(fit, model)
  #   indices <- data.frame(Value = fitMeasures(newfit)) %>%
  #     tibble::rownames_to_column("Index") %>%
  #     tidyr::spread_("Index", "Value") %>%
  #     cbind(data.frame(model = model,
  #                      n_outcomes = n,
  #                      n_links = nrow(lavaan::lavInspect(fit, "est")$beta)))
  #   out <- rbind(out, indices)
  #   return(out)
  # }
  #
  # out <- data.frame()
  # for(i in 1:samples){
  #   if(verbose==TRUE){
  #     print(paste0(round(i/samples*100), "%"))
  #   }
  #   out <- tryCatch(
  #     get_solution(possibilities, fit, out),
  #     error=function(e) out,
  #     warning=function(w) out)
  # }
  #
  # out %>%
  #   distinct() %>%
  #   return()
# }







# models <- apply(data, 1, paste, collapse="\n")


# possible_outcomes <- c()
# for(i in 1:length(vars)){
#   possible_outcomes <- c(possible_outcomes, combn(vars, i, simplify=FALSE))
# }
#
# for(i in possible_outcomes){
#
#   # combinations <- refdata(possibilities)
#
# }

