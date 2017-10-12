context("correlation")

test_that("Ccorrelations work", {
  df <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 5, 1, 4, 2),
    C = c(2, 5, 1, 6, 2)
  )

  # Pearson
  output <- psycho::correlation(df)
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, -0.1, tol = 0.1)

  # Spearman
  output <- psycho::correlation(df, method = "spearman")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, -0.10, tol = 0.1)

  # Partial
  output <- psycho::correlation(df, type = "partial", adjust = "holm")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, -0.43, tol = 0.1)

  # Semipartial
  df2 <- data.frame(
    A = c(1, 2, 3, 4, 5),
    B = c(2, 5, 1, 4, 2)
  )
  output <- psycho::correlation(df, type = "semi", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, -0.16, tol = 0.1)

  # Plot
  plot <- output$plot()
  testthat::expect_equal(plot[1, 1], 1, tol = 0.1)
})
