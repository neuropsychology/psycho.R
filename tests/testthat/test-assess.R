context("assess")

test_that("It works", {
  x <- assess(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.019, tol = 0.02)
})
