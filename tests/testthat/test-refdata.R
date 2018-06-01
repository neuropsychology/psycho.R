context("refdata")

test_that("If it works.", {
  testthat::expect_equal(nrow(psycho::refdata(psycho::affective, target = "Sex")), 2)
  testthat::expect_equal(nrow(psycho::refdata(iris, length.out = 2)), 48)
  testthat::expect_equal(nrow(psycho::refdata(iris, target = "Sepal.Length", length.out = 2, factors = "combinations")), 6)
  testthat::expect_equal(nrow(psycho::refdata(iris, target = "Species", length.out = 2, factors = "combinations")), 3)
})
