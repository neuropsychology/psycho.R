context("standardize")

test_that("Correct Value", {
  df <- data.frame(
    V1 = rnorm(100, 30, .2),
    V2 = runif(100, 3, 5),
    V3 = rnorm(100, 100, 10)
  )
  dfZ <- standardize(df)
  testthat::expect_equal(mean(dfZ$V1), 0)

  dfZ <- standardize(df, except = "V3")
  testthat::expect_equal(mean(dfZ$V2), 0)

  dfZ <- standardize(df, except = c("V1", "V2"))
  testthat::expect_equal(mean(dfZ$V3), 0)

  dfZ <- standardize(df$V1)
  testthat::expect_equal(mean(dfZ), 0)

  dfZ <- standardize(df, subset = c("V1", "V2"))
  testthat::expect_equal(mean(dfZ$V1), 0)

  dfZ <- standardize(df, subset = "V1", except="V3")
  testthat::expect_equal(mean(dfZ$V1), 0)
})
