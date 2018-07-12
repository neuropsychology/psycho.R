context("analyze.stanreg")

test_that("If it works.", {
  # Fit
  library(rstanarm)
  library(psycho)

  set.seed(666)

  fit <- rstanarm::stan_glm(
    vs ~ mpg * cyl,
    data = mtcars,
    family = binomial(link = "logit"),
    prior = NULL,
    chains = 1, iter = 1000, seed = 666
  )

  model <- psycho::analyze(fit)
  values <- psycho::values(model)
  testthat::expect_equal(round(values$effects$mpg$median, 2), -0.6, tolerance = 0.10)

  model <- psycho::analyze(fit, effsize = TRUE)
  values <- psycho::values(model)
  testthat::expect_equal(round(values$effects$mpg$std_median, 2), 0.13, tolerance = 0.10)


  # Random
  fit <- rstanarm::stan_glmer(
    Sepal.Length ~ Sepal.Width + (1 | Species),
    data = iris,
    chains = 1, iter = 1000, seed = 666
  )

  model <- psycho::analyze(fit, effsize = FALSE)
  values <- psycho::values(model)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$median, 2), 0.79,
    tolerance = 0.05
  )



  # standardized
  data <- psycho::standardize(iris)
  fit <- rstanarm::stan_glm(Sepal.Length ~ Sepal.Width + Petal.Width,
    data = data,
    prior = rstanarm::normal(0, 1, autoscale = FALSE),
    chains = 1, iter = 1000, seed = 666
  )
  results <- psycho::analyze(fit)
  testthat::expect_equal(
    round(results$values$effects$Sepal.Width$median, 2), 0.21,
    tolerance = 0.025
  )
  results <- psycho::analyze(fit, effsize = TRUE)
  testthat::expect_equal(
    round(results$values$effects$Sepal.Width$median, 2), 0.21,
    tolerance = 0.025
  )



  # Other algorithms
  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "meanfield"
  )

  results <- psycho::analyze(fit)
  values <- psycho::values(results)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$median, 2), -0.46,
    tolerance = 0.1
  )

  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "fullrank"
  )

  results <- psycho::analyze(fit)
  values <- psycho::values(results)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$median, 2), -0.12,
    tolerance = 0.1
  )

  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "optimizing"
  )
  testthat::expect_error(psycho::analyze(fit))
})
