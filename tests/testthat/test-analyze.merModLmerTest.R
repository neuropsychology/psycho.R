context("analyze.merModLmerTest")

test_that("If it works.", {
  # Fit
  require(lmerTest)

  fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1|Species), data = iris)

  model <- psycho::analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$Sepal.Width$Coef, 2), 0.8,
                         tolerance = 0.05)
})
