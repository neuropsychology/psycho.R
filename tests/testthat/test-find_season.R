context("find_season")

test_that("Correct date", {
  dates <- c("2017-02-15", "2017-05-15", "2017-08-15", "2017-11-15")
  dates <- find_season(dates)
  expect_equal(as.character(dates[1]), "Winter")
})
