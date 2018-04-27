context("overlap")

test_that("Correct", {
  x <- rnorm(1000, 1, 0.5)
  y <- rnorm(1000, 0, 1)
  testthat::expect_equal(overlap(x, y), 0.43, tolerance = 0.05)
})
