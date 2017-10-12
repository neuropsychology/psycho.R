context("normalize")

test_that("Correct Value", {
  x <- data.frame(x = c(8, 10, 12), y = c("a", "b", "c"))
  x <- psycho::normalize(x)
  testthat::expect_equal(mean(x$x), 0)

  x <- data.frame(x = c(8, 10, 12))
  x <- psycho::normalize(x)
  testthat::expect_equal(mean(x$x), 0)
})
