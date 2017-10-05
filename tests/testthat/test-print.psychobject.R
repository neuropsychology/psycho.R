context("print.psychobject")

test_that("It works", {
  output <- list(text = 1)
  class(output) <- c("psychobject", "list")
  text <- print(output)
  expect_equal(text, 1)
})
