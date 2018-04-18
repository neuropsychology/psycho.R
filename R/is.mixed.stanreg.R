#' Check if model includes random effects.
#'
#' Check if model is mixed.
#'
#' @param fit Model.
#' @param ... Arguments passed to or from other methods.
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @export
is.mixed.stanreg <- function(fit, ...) {
  mixed <- tryCatch({
    broom::tidy(fit, parameters = "varying")
    TRUE
  }, error = function(e) {
    FALSE
  })
  return(mixed)
}
