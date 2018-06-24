context("standardize")

test_that("Correct Value", {
  library(psycho)

  df <- data.frame(
    Participant = as.factor(rep(1:25, each = 4)),
    Condition = base::rep_len(c("A", "B", "C", "D"), 100),
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

  dfZ <- standardize(df, subset = "V1", except = "V3")
  testthat::expect_equal(mean(dfZ$V1), 0)

  dfZ <- standardize(dplyr::group_by(df, Participant))
  testthat::expect_equal(mean(dfZ$V1), 0)
})
