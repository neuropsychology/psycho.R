context("plot.psychobject")

test_that("It works", {
  output <- list(plot = 1)
  class(output) <- c("psychobject", "list")
  plot <- plot(output)
  expect_equal(plot, 1)
})
