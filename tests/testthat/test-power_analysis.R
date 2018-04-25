context("power_analysis")

test_that("Correct", {
  fit <- lm(Sepal.Length ~ Sepal.Width, data=iris)
  results <- power_analysis(fit, n_max=300, n_min=150, step=50, n_batch=1)

  testthat::expect_equal(nrow(results), 8, tolerance = 0.01)
})
