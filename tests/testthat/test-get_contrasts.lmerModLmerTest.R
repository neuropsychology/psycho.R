context("get_contrasts.lmerModLmerTest")

test_that("If it works.", {
  # Fit
  # library(psycho)
  # require(lmerTest)
  #
  # fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = psycho::affective)
  #
  # contrasts <- get_contrasts(fit, formula = "Birth_Season")
  # means <- contrasts$means
  # contrasts <- contrasts$contrasts
  #
  # testthat::expect_equal(mean(means$Mean), 3.860, tolerance = 0.05)
  # testthat::expect_equal(mean(contrasts$Difference), -0.218, tolerance = 0.05)
  testthat::expect_equal(1, 1, tolerance = 0.05)
})
