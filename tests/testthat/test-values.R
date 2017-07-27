context("values")

test_that("It works", {
  output <- list(values=1)
  class(output) <- c("psycho", "list")
  values <- values(output)
  expect_equal(values, 1)
})
