context("analyze.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)


  fit <- rstanarm::stan_glm(
    vs ~ mpg * cyl,
    data = mtcars,
    family = binomial(link = "logit"),
    prior = NULL,
    seed = 666
  )

  model <- psycho::analyze(fit)
  values <- psycho::values(model)
  testthat::expect_equal(round(values$mpg$median, 2), -0.64, tolerance = 0.10)


  fit <- rstanarm::stan_glmer(
    Sepal.Length ~ Sepal.Width + (1 | Species),
    data = iris,
    seed = 666
  )

  model <- psycho::analyze(fit, effsize = T)
  values <- psycho::values(model)
  testthat::expect_equal(
    round(values$Sepal.Width$median, 2), 0.79,
    tolerance = 0.05
  )
})
