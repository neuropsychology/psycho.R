context("overlap")

test_that("Correct", {
  x <- psycho::rnorm_perfect(10, 0, 1)
  testthat::expect_equal(mean(x), 0, tolerance = 0.02)

  x <- psycho::rnorm_perfect(10, 0, 1, method = "average")
  testthat::expect_equal(mean(x), 0, tolerance = 0.05)
})
