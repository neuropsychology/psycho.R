context("interpret_bf")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_bf(3), "moderate evidence in favor of")
  testthat::expect_equal(psycho::interpret_bf(1 / 3), "moderate evidence against")
})
