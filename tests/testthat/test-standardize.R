context("standardize")

test_that("Correct Value", {
  library(psycho)

  set.seed(666)
  df <- data.frame(
    Participant = as.factor(rep(1:25, each = 4)),
    Condition = base::rep_len(c("A", "B", "C", "D"), 100),
    V1 = rnorm(100, 30, .2),
    V2 = runif(100, 3, 5),
    V3 = rnorm(100, 100, 10)
  )

  dfZ <- standardize(df)
  testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)

  dfZ <- standardize(df, except = "V3")
  testthat::expect_equal(mean(dfZ$V2), 0, tol = 0.01)

  dfZ <- standardize(df, except = c("V1", "V2"))
  testthat::expect_equal(mean(dfZ$V3), 0, tol = 0.01)

  dfZ <- standardize(df$V1)
  testthat::expect_equal(mean(dfZ), 0, tol = 0.01)

  dfZ <- standardize(df, subset = c("V1", "V2"))
  testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)

  dfZ <- standardize(df, subset = "V1", except = "V3")
  testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)

  dfZ <- standardize(dplyr::group_by(df, Participant))
  testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)

  dfN <- standardize(df, except = "V3", normalize = TRUE)
  testthat::expect_equal(mean(dfN$V2), 0.533, tol = 0.01)


  # Models
  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "meanfield"
  )

  std <- standardize(fit, method="posterior")
  testthat::expect_equal(mean(std), -0.24, tol = 0.02)

  std <- standardize(fit, method="sample")
  testthat::expect_equal(mean(std), 1.34, tol = 0.02)

  fit <- lm(
    Sepal.Length ~ Sepal.Width,
    data = iris
  )

  std <- standardize(fit, method="posthoc")
  testthat::expect_equal(mean(std$Coef_std), -0.059, tol = 0.01)

})
