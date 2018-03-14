context("mellenbergh.test")

test_that("Correct Value", {
  x <- mellenbergh.test(t0 = 4,
                        t1 = 12,
                        controls = c(0, -2, 5, 2, 1, 3, -4, -2))

  testthat::expect_equal(x$z, 1.90, tol=0.2)


  x <- mellenbergh.test(t0 = 4,
                        t1 = 12,
                        controls = 2.54)

  testthat::expect_equal(x$z, 2.22, tol=0.2)
})
