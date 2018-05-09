context("crawford.test")

test_that("Correct Value", {

# bayesian ----------------------------------------------------------------


  x <- crawford.test(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.019, tol = 0.02)

  x <- crawford.test(
    patient = -10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.019, tol = 0.02)



# frequentist -------------------------------------------------------------


  x <- crawford.test.freq(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, 3.05, tol = 0.2)

  x <- crawford.test.freq(
    patient = -10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, -3.3, tol = 0.2)

  x <- crawford.test.freq(
    patient = 7,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, 2.10, tol = 0.2)

  x <- crawford.test.freq(
    patient = 0,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, -0.12, tol = 0.2)
})
