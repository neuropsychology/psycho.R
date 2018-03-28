context("get_contrasts.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data = df)

  contrasts <- psycho::get_contrasts(fit, "Salary")
  means <- contrasts$means
  contrasts <- contrasts$contrasts

  testthat::expect_equal(mean(means$Mean), 4.876, tolerance = 0.05)
  testthat::expect_equal(mean(contrasts$Mean), -0.136, tolerance = 0.05)
})
