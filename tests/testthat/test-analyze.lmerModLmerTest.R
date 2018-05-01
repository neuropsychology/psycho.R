context("analyze.lmerModLmerTest")

test_that("If it works.", {
  # Fit
  library(lmerTest)

  fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$Coef, 2), 0.8,
    tolerance = 0.05
  )
})
