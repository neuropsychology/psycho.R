context("analyze.merMod")

test_that("If it works.", {
  # Fit
  require(lme4)

  x <- lme4::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)

  model <- psycho::analyze(x)
  values <- values(model)
  testthat::expect_equal(
    round(values$Sepal.Width$Coef, 2), 0.8,
    tolerance = 0.05
  )
})
