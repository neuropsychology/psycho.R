context("model_to_priors")

test_that("Correct", {
  fit <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data=iris)
  priors <- psycho::model_to_priors(fit)
  testthat::expect_equal(length(priors), 3)
})
