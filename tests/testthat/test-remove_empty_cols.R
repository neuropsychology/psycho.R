context("remove_empty_cols")

test_that("Correct", {
  df <- data.frame(A = c(1, 2 ,3),
                   B = c(1, 2, 3))
  df$C <- NA

  testthat::expect_equal(ncol(psycho::remove_empty_cols(df)), 2)
})
