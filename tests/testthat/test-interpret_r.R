context("interpret_r")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_r(0), "very small, and negative")
  testthat::expect_equal(psycho::interpret_r(0, rules = "evans1996"), "very weak, and negative")
})
