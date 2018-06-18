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

})
