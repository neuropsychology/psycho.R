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
#' analyze(fit)
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
