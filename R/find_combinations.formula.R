#' Generate all combinations of predictors of a formula.
#'
#' Generate all combinations of predictors of a formula.
#'
#' @param object Formula.
#' @param interaction Include interaction term.
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
#'
#' @author \href{https://dominiquemakowski.github.io/}{Dominique Makowski}
#'
#' @method find_combinations formula
#' @importFrom utils combn
#' @importFrom stats terms
#' @export
find_combinations.formula <- function(object, interaction=TRUE, ...) {

  # Extract infos
  formula <- object
  vars <- attributes(terms(formula))$term.labels
  outcome <- all.vars(formula)[1]
  pred <- vars[!grepl("\\|", vars)]
  if(length(vars[grepl("\\|", vars)]) > 0){
    random <- paste0(" + (", vars[grepl("\\|", vars)], ")")
  } else{
    random <- ""
  }

  # Generate combinations
  n <- length(pred)

  id <- unlist(
    lapply(1:n,
           function(i) combn(1:n,i,simplify=FALSE)
    )
    ,recursive=FALSE)

  combinations <- sapply(id,function(i)
    paste(paste(pred[i], collapse=" + "))
  )


  # Generate interactions
  if (interaction==TRUE){
    for(comb in combinations){
      n_signs <- stringr::str_count(comb, "\\+")
      if(n_signs > 0){
        new_formula <- comb
        for(i in 1:n_signs){
          new_formula <- stringr::str_replace(new_formula, "\\+", "*")
          combinations <- c(combinations, new_formula)
        }
      }
    }
  }

  combinations <- paste0(outcome, " ~ ", combinations, paste0(random, collapse=""))
  return(combinations)
}

