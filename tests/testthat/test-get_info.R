context("get_info")

test_that("Correct Value", {
  fit <- lme4::glmer(vs ~ wt + (1 | gear), data = mtcars, family = "binomial")
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "vs")

  fit <- lme4::lmer(hp ~ wt + (1 | gear), data = mtcars)
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "hp")

  fit <- glm(vs ~ wt, data = mtcars, family = "binomial")
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "vs")

  fit <- lm(hp ~ wt, data = mtcars)
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "hp")

  fit <- rstanarm::stan_glm(hp ~ wt, data = mtcars)
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "hp")

  outcome <- "hp"
  fit <- lm(paste(outcome, " ~ wt"), data = mtcars)
  info <- get_info(fit)
  testthat::expect_equal(info$outcome, "hp")
})
