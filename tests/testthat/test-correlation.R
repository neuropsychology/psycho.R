context("correlation")

test_that("Ccorrelations work", {
  df <- attitude[c("rating", "complaints", "privileges", "learning")]


  # Pearson
  output <- psycho::correlation(df)
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.82, tol = 0.1)

  # Spearman
  output <- psycho::correlation(df, method = "spearman")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.83, tol = 0.1)

  # Partial
  output <- psycho::correlation(df, type = "partial", adjust = "holm")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.72, tol = 0.1)

  # Semi
  output <- psycho::correlation(df, type = "semi", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.53, tol = 0.1)

  # Dual
  df2 <- attitude[c("raises", "critical")]

  output <- psycho::correlation(df, df2, type = "full", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value,  0.66, tol = 0.1)

  type = "semi"
  adjust = "none"
  method="pearson"
  output <- psycho::correlation(df, df2, type = "semi", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.46, tol = 0.1)

  plot <- plot(output)
  testthat::expect_equal(length(plot), 10, tol = 0.1)

  # Other
  output <- psycho::correlation(df, type = "dupa", adjust = "holm")
  testthat::expect_null(output)

  # Plot
  plot <- plot(correlation(df))
  testthat::expect_equal(length(plot), 10, tol = 0.1)
})
