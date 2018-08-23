#' Simulates data for single or multiple regression.
#'
#' Simulates data for single or multiple regression.
#'
#' @param sample Desired sample size.
#' @param coefs Desired theorethical coefs. Can be a single value or a list.
#' @param error The error (standard deviation of gaussian noise).
#'
#' @examples
#' library(psycho)
#'
#' data <- simulate_data_regression(sample=50, coefs=c(0, 0.5), error=0.1)
#' lm(y ~ ., data=data)
#'
#' @details See https://stats.stackexchange.com/questions/59062/multiple-linear-regression-simulation
#'
#' @author TPArrow
#'
#' @export
simulate_data_regression <- function(sample=10, coefs=0, error=0){

  n_var = length(coefs)
  X = matrix(0, ncol=n_var, nrow=sample)

  beta = as.matrix(coefs)

  for (i in 1:n_var){
    X[,i] = rnorm(sample, 0, 1)
  }

  y = X %*% beta + rnorm(sample, 0, error)
  data = data.frame(X=X)
  names(data) <- paste0("V", 1:n_var)
  data$y <- y

  return(data)
}

