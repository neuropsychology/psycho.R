context("is.mixed.stanreg")

test_that("Correct Value", {
  library(rstanarm)
  fit <- rstanarm::stan_glm(Sepal.Length ~ Petal.Length, data = iris, iter = 100)
  testthat::expect_equal(is.mixed(fit), FALSE)
  fit <- rstanarm::stan_lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris, iter = 100)
  testthat::expect_equal(is.mixed(fit), TRUE)
})
