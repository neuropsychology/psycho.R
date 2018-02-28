context("dprime")

test_that("Correct Value", {

  testthat::expect_equal(dprime(9, 1, 2, 7)$dprime, 1.65, tolerance=0.1)
  testthat::expect_equal(dprime(9, 0, 0, 0)$dprime, 1.74, tolerance=0.1)
  testthat::expect_equal(dprime(0, 10, 0, 0)$dprime, -1.28, tolerance=0.1)
})
