context("probs_to_odds")

test_that("Correct", {
  testthat::expect_equal(probs_to_odds(0.75), 3, tolerance = 0.01)
  testthat::expect_equal(probs_to_odds(0.75, log = TRUE), 1.098, tolerance = 0.01)
})
