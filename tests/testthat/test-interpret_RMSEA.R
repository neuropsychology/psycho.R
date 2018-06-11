context("interpret_RMSEA")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_RMSEA(0.04), "good")
  testthat::expect_equal(psycho::interpret_RMSEA(0.05), "acceptable")
  testthat::expect_equal(psycho::interpret_RMSEA(0.08), "poor")
})
