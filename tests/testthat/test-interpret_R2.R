context("interpret_bf")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_R2(0.5), "large")
  testthat::expect_true(is.na(psycho::interpret_R2(-5)))
})
