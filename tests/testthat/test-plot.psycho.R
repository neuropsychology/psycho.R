context("plot.psycho")

test_that("It works", {
  output <- list(plot=1)
  class(output) <- c("psycho", "list")
  plot <- plot(output)
  expect_equal(plot, 1)
})
