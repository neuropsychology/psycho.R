context("assess")

test_that("It works", {
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
    verbose=FALSE
  )

  testthat::expect_equal(x[[1]]$values$p, 0.16, tol = 0.05)
})
