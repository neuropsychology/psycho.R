context("create_intervals")

test_that("Correct Value", {
  x <- psycho::rnorm_perfect(1000)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, 3))), 3)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, length=100))), 2)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, 3, equal_range=FALSE))), 3)
  testthat::expect_true(is.numeric(psycho::create_intervals(x, 3, labels="median")))
  testthat::expect_true(is.numeric(psycho::create_intervals(x, 3, labels=FALSE)))

})
