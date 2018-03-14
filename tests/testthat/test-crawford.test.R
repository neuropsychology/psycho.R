context("crawford.test")

test_that("Correct Value", {
  x <- crawford.test(case = 10,
                     controls = c(0, -2, 5, 2, 1, 3, -4, -2),
                     verbose=T)

  testthat::expect_equal(x$t, 3.05, tol=0.2)
})
