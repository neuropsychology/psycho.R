context("interpret_d")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_d(0), "very small")
  testthat::expect_equal(psycho::interpret_d(0, rules = "sawilowsky2009"), "tiny")

  testthat::expect_equal(psycho::interpret_d_posterior(c(0.1, 0.1, 0.1, 0.1))$values$large, 0)
})
