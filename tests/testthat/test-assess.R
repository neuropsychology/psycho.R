context("neuropsychological tests")




test_that("assess", {
  x <- assess(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.018, tol = 0.02)

  x <- assess(
    patient = 10,
    mean = 8,
    sd = 2,
    n = 10
  )

  testthat::expect_equal(x$values$p, 0.18, tol = 0.02)

  x <- assess(
    patient = c(10, 12),
    mean = 8,
    sd = 2,
    verbose = FALSE
  )

  testthat::expect_equal(x[[1]]$values$p, 0.16, tol = 0.05)
})





test_that("crawford.test", {

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







test_that("crawford.test", {
  x <- crawford_dissociation.test(
    case_X = 142,
    case_Y = 7,
    controls_X = c(100, 125, 89, 105, 109, 99),
    controls_Y = c(7, 8, 9, 6, 7, 10)
  )

  testthat::expect_equal(x$t, 2.1, tol = 0.02)
})










test_that("mellenbergh.test", {
  x <- mellenbergh.test(
    t0 = 4,
    t1 = 12,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$z, 1.90, tol = 0.2)


  x <- mellenbergh.test(
    t0 = 4,
    t1 = 12,
    controls = 2.54
  )

  testthat::expect_equal(x$values$z, 2.22, tol = 0.2)

  x <- mellenbergh.test(t0 = 4, t1 = 12, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
  testthat::expect_equal(x$values$z, 1.90, tol = 0.1)
  x <- mellenbergh.test(t0 = 8, t1 = 2, controls = 2.6)
  testthat::expect_equal(x$values$z, -1.63, tol = 0.1)
})












