context("interpret_bf")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_bf(3), "moderate evidence in favour of")
  testthat::expect_equal(psycho::interpret_bf(1 / 3), "moderate evidence against")
})
