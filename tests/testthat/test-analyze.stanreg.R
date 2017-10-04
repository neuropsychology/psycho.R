context("analyze.stanreg")

test_that("If it works.", {
  # Fit
  require(rstanarm)

  set.seed(666)
  fit <- rstanarm::stan_glm(vs ~ mpg * cyl,
    data=mtcars,
    family = binomial(link = "logit"),
    prior=NULL)

  model <- analyze(fit)
  values <- values(model)
  expect_equal(round(values$mpg$median, 2), -0.68)
})
