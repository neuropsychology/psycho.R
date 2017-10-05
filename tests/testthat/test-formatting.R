context("formatting")

test_that("Formatting works as expected", {
  expect_equal(format_digit(0), "0")
  expect_equal(format_digit(1), 1)
  expect_equal(format_digit(1.101), 1.1)
})
