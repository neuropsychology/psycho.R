context("analyze.htest")

test_that("If it works.", {
  library(psycho)

  df <- psycho::affective

  x <- t.test(df$Adjusting, df$Concealing)
  rez <- psycho::analyze(x)
  testthat::expect_equal(ncol(summary(rez)), 6)

  x <- cor.test(df$Adjusting, df$Concealing)
  rez <- psycho::analyze(x)
  testthat::expect_equal(ncol(summary(rez)), 6)

  x <- t.test(df$Adjusting ~ df$Sex)
  rez <- psycho::analyze(x)
  testthat::expect_equal(ncol(summary(rez)), 6)
})
