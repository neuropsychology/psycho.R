context("interpret_R2")

test_that("Correct Value", {
  testthat::expect_equal(psycho::interpret_R2(0.2), "medium")
  testthat::expect_equal(psycho::interpret_R2(0.2, rules="chin1998"), "small")
  testthat::expect_equal(psycho::interpret_R2(0.2, rules="hair2013"), "very small")
  testthat::expect_true(is.na(psycho::interpret_R2(-5)))

  testthat::expect_equal(psycho::interpret_R2_posterior(c(0.2, 0.2, 0.2))$values$medium, 1)
  testthat::expect_equal(psycho::interpret_R2_posterior(c(0.1, 0.2, 0.3, 0.4))$values$large, 0.5)
})
