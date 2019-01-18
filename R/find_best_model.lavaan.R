#' Returns all combinations of lavaan models with their indices of fit.
#'
#' Returns all combinations of lavaan models with their indices of fit.
#'
#' @param fit A lavaan object.
#' @param latent Copy/paste the part related to latent variables loadings.
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
#' model <- " visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9
#' visual ~ textual
#' textual ~ speed"
#' fit <- lavaan::sem(model, data = HolzingerSwineford1939)
#' 
#' models <- find_best_model(fit, latent = "visual  =~ x1 + x2 + x3
#' textual =~ x4 + x5 + x6
#' speed   =~ x7 + x8 + x9")
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @import dplyr
#'
#' @method find_best_model lavaan
#' @export
find_best_model.lavaan <- function(fit, latent = "", samples = 1000, verbose = FALSE, ...) {
  update_model <- function(fit, latent, model) {
    newfit <- update(fit, paste0(latent, "\n", model))

    indices <- data.frame(Value = lavaan::fitMeasures(newfit)) %>%
      tibble::rownames_to_column("Index") %>%
      tidyr::spread_("Index", "Value") %>%
      cbind(data.frame(
        model = model,
        n_links = nrow(lavaan::lavInspect(fit, "est")$beta)
      ))
    return(indices)
  }

  vars <- row.names(lavaan::lavInspect(fit, "est")$beta)
  # info <- fit@Model

  data <- data.frame()
  for (outcome in vars) {
    remaning_vars <- vars[!stringr::str_detect(vars, outcome)]
    combinations <- c()
    for (y in 1:length(remaning_vars)) {
      combinations <- c(combinations, combn(remaning_vars, y, simplify = FALSE))
    }
    combinations <- sapply(combinations, paste0, collapse = "+")
    combinations <- paste0(outcome, "~", combinations)
    x <- data.frame(A = combinations)
    names(x) <- c(outcome)
    if (nrow(data) == 0) {
      data <- x
    } else {
      data <- cbind(data, x)
    }
  }

  data <- rbind(data, head(data[NA, ], 1))
  data[] <- lapply(data, as.character)
  data[is.na(data)] <- ""
  rownames(data) <- NULL

  out <- data.frame()
  for (i in 1:samples) {
    if (verbose == TRUE) {
      cat(".")
    }
    model <- ""
    for (var in names(data)) {
      model <- paste0(model, sample(data[[var]], 1), "\n")
    }

    if (!model %in% out$model) {
      out <- tryCatch(
        rbind(out, update_model(fit, latent, model)),
        error = function(e) out,
        warning = function(w) out
      )
    }
  }
  return(out)
}
