context("interpret_bf")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_bf(3), "moderate evidence (BF = 3) in favour of")
  testthat::expect_equal(psycho::interpret_bf(1 / 3), "moderate evidence (BF = 3) against")
})
