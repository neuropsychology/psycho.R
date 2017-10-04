context("normalize")

test_that("Correct Value", {
  x <- as.vector(c(8, 10, 12))
  x <- normalize(x)
  expect_equal(mean(x$V1), 0)
})
