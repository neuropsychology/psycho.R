context("interpret_odds")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_odds(0), "very small")
  testthat::expect_equal(psycho::interpret_odds(0, log = TRUE), "very small")
  testthat::expect_equal(psycho::interpret_odds(5, log = TRUE), "large")
  testthat::expect_equal(psycho::interpret_odds(5, log = TRUE, rules = "cohen1988"), "large")

  testthat::expect_equal(psycho::interpret_odds_posterior(c(5, 5, 5, 5))$values$large, 0)
})
