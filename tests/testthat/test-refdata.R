context("refdata")

test_that("If it works.", {
  newdata <- psycho::refdata(psycho::affective, target = "Sex")

  testthat::expect_equal(nrow(newdata), 2)
})
