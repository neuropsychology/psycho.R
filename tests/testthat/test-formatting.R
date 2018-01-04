context("formatting")

test_that("Formatting works as expected", {
  testthat::expect_equal(format_digit(0.0008), "0")
  testthat::expect_equal(format_digit(0.00000), "0")
  testthat::expect_equal(format_digit(0.005887), "0.0059")
  testthat::expect_equal(format_digit(0.0405), "0.040")
  testthat::expect_equal(format_digit(-0.005887), "-0.0059")
  testthat::expect_equal(format_digit(-0.0405), "-0.040")
  testthat::expect_equal(format_digit(0.405), "0.40")
  testthat::expect_equal(format_digit(1.1587), "1.16")
  testthat::expect_equal(format_digit(12), "12")
  testthat::expect_equal(format_digit(1.101), "1.10")
})
