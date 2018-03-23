context("crawford.test")

test_that("Correct Value", {
  x <- crawford_dissociation.test(
    case_X = 142,
    case_Y = 7,
    controls_X = c(100, 125, 89, 105, 109, 99),
    controls_Y = c(7, 8, 9, 6, 7, 10)
  )

  testthat::expect_equal(x$t, 2.1, tol = 0.02)
})
