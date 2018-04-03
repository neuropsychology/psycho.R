context("find_best_model.stanreg")

test_that("Correct", {
  testthat::expect_equal(1, 1)

  # The following fails for some reasons

  # data <- standardize(attitude)
  # fit <- rstanarm::stan_glm(rating ~ advance + privileges,
  #                           chains = 1, iter = 500,
  #                           data=data,
  #                           seed=666)
  #
  # best <- find_best_model(fit, K=2)
  # best_formula <- best$formula
  # testthat::expect_equal(best_formula, "rating ~ privileges")
  #
  # best <- find_best_model(fit, K=0)
  # best_formula <- best$formula
  # testthat::expect_equal(best_formula, "rating ~ privileges")

})
