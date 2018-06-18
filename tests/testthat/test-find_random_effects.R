context("find_random_effects")

test_that("Correct", {
  f <- as.formula("Y ~ A + B + C + D + (1|E)")
  rf <- psycho::find_random_effects(f)
  testthat::expect_equal(rf, "(1|E)")
})
