context("hdi")

test_that("Correct Value", {
  x <- attitude$rating
  results <- psycho::hdi(x, 0.95)

  testthat::expect_equal(results$values$HDImin, 40)
  testthat::expect_equal(length(plot(results)), 9)
})
