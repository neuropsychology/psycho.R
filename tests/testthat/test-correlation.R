context("correlation")

test_that("Correct correlation", {
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 5, 1, 4, 2),
    C = c(2, 5, 1, 6, 2)
  )
  output <- psycho::correlation(df)
  value <- output$values$r[2, 1]
  value <- round(value, 1)
  expect_equal(value,-0.1)
})
