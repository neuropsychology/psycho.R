context("interpret_odds")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_odds(0), "very small")
  testthat::expect_equal(psycho::interpret_odds(0, log = TRUE), "very small")
})
