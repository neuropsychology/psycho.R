context("print.psycho")

test_that("It works", {
  output <- list(text=1)
  class(output) <- c("psycho", "list")
  text <- print(output)
  expect_equal(text, 1)
})
