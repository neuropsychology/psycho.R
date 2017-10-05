context("analyze.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)

  fit <- rstanarm::stan_glm(vs ~ mpg * cyl,
    data=mtcars,
    family=binomial(link = "logit"),
    prior=NULL,
    seed=666)

  model <- psycho::analyze(fit)
  values <- values(model)
  expect_equal(round(values$mpg$median, 2), -0.64, tolerance = 0.05)
})
