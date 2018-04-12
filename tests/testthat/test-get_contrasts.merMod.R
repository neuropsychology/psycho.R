context("get_contrasts.merMod")

test_that("If it works.", {
  # Fit
  require(rstanarm)

  df <- psycho::affective
  fit <- lme4::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = affective)

  contrasts <- psycho::get_contrasts(fit, formula = "Birth_Season")
  means <- contrasts$means
  contrasts <- contrasts$contrasts

  testthat::expect_equal(mean(means$Mean), 3.860, tolerance = 0.05)
  testthat::expect_equal(mean(contrasts$Difference), -0.218, tolerance = 0.05)
})
