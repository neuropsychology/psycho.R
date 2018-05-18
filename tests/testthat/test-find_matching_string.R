context("find_matching_string")

test_that("Correct", {
  testthat::expect_equal(find_matching_string("Hwo rea ouy", c("How are you", "Not this word", "Nice to meet you")), "How are you")
})
