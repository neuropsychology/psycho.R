context("analyze.lm")

test_that("If it works.", {
  library(psycho)

  # GLM
  fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$Sepal.Length$Coef, 2), -0.06, tolerance = 0.01)

  # test summary
  summa <- summary(model, round = 2)
  testthat::expect_equal(nrow(summa), 2)


  # Poly
  fit <- lm(Sepal.Width ~ poly(Sepal.Length, 2), data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$`poly(Sepal.Length, 2)2`$Coef, 2), 0.82, tolerance = 0.01)
})
