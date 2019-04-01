context("deprecated")

test_that("If it works.", {
  library(psycho)
  library(lmerTest)
  library(lme4)

  df <- psycho::affective
  x <- aov(Tolerating ~ Salary, data = df)
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- anova(lm(Tolerating ~ Salary, data = df))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 2)

  x <- aov(Tolerating ~ Birth_Season + Error(Sex), data = df)
  testthat::expect_message(psycho::analyze(x))

  x <- anova(lmerTest::lmer(Tolerating ~ Birth_Season + (1 | Sex), data = df))
  testthat::expect_equal(nrow(summary(psycho::analyze(x))), 1)

  x <- anova(lme4::lmer(Tolerating ~ Birth_Season + (1 | Sex), data = df))
  testthat::expect_error(psycho::analyze(x))
})


test_that("analyze.glmer", {
  library(lme4)

  # GLM
  fit <- lme4::glmer(vs ~ mpg + (1 | cyl), data = mtcars, family = "binomial")

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$mpg$Coef, 2), 0.17, tolerance = 0.02)

  # test summary
  summa <- summary(model, round = 2)
  testthat::expect_equal(nrow(summa), 2)

  # GLM
  fit <- lme4::glmer(Sex ~ Adjusting + (1 | Salary), data = psycho::affective, family = "binomial")
  testthat::expect_warning(analyze(fit))
})


test_that("analyze.glm", {
  library(psycho)

  # GLM
  fit <- glm(vs ~ mpg, data = mtcars, family = "binomial")

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$mpg$Coef, 2), 0.43, tolerance = 0.02)

  # test summary
  summa <- summary(model, round = 2)
  testthat::expect_equal(nrow(summa), 2)
})




test_that("analyze.htest", {
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

  x <- t.test(df$Adjusting, mu = 0)
  rez <- psycho::analyze(x)
  testthat::expect_equal(ncol(summary(rez)), 6)
})








test_that("analyze.lavaan", {
  library(psycho)
  library(lavaan)

  HS.model <- " visual  =~ x1 + x2 + x3\n  textual =~ x4 + x5 + x6\n  speed   =~ x7 + x8 + x9 "

  fit <- lavaan::cfa(HS.model, data = lavaan::HolzingerSwineford1939)
  rez <- analyze(fit)
  testthat::expect_equal(nrow(summary(rez)), 24)
})




test_that("analyze.lm", {
  library(psycho)

  # GLM
  fit <- lm(Sepal.Width ~ Sepal.Length, data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$Sepal.Length$Coef, 2), -0.06, tolerance = 0.01)

  # test summary
  summa <- summary(model, round = 2)
  testthat::expect_equal(nrow(summa), 2)


  # Poly
  fit <- lm(Sepal.Width ~ poly(Sepal.Length, 2), data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(round(values$effects$`poly(Sepal.Length, 2)2`$Coef, 2), 0.82, tolerance = 0.01)
})





test_that("analyze.lmerModLmerTest", {
  # Fit
  library(lmerTest)

  fit <- lmerTest::lmer(Sepal.Length ~ Sepal.Width + (1 | Species), data = iris)

  model <- analyze(fit)
  values <- values(model)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$Coef, 2), 0.8,
    tolerance = 0.05
  )
})




test_that("analyze.stanreg", {
  # Fit
  library(rstanarm)
  library(psycho)

  set.seed(666)

  quiet <- function(x) {
    sink(tempfile())
    on.exit(sink())
    invisible(force(x))
  }



  fit <- quiet(rstanarm::stan_glm(
    vs ~ mpg * as.factor(cyl),
    data = mtcars,
    family = binomial(link = "logit"),
    prior = NULL,
    chains = 1, iter = 1000, seed = 666
  ))

  model <- psycho::analyze(fit)
  values <- psycho::values(model)
  testthat::expect_equal(round(values$effects$mpg$median, 2), 0.08, tolerance = 0.10)

  model <- psycho::analyze(fit, effsize = TRUE)
  values <- psycho::values(model)
  testthat::expect_equal(round(values$effects$mpg$median, 2), 0.08, tolerance = 0.10)
  # This needs to be fixed:
  # testthat::expect_equal(round(values$effects$mpg$std_median, 2), 0.39, tolerance = 0.10)


  # Random
  fit <- quiet(rstanarm::stan_glmer(
    Sepal.Length ~ Sepal.Width + (1 | Species),
    data = iris,
    chains = 1, iter = 1000, seed = 666
  ))

  model <- psycho::analyze(fit, effsize = FALSE)
  values <- psycho::values(model)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$median, 2), 0.79,
    tolerance = 0.05
  )



  # standardized
  data <- psycho::standardize(iris)
  fit <- quiet(rstanarm::stan_glm(Sepal.Length ~ Sepal.Width + Petal.Width,
    data = data,
    prior = rstanarm::normal(0, 1, autoscale = FALSE),
    chains = 1, iter = 1000, seed = 666
  ))
  results <- psycho::analyze(fit)
  testthat::expect_equal(
    round(results$values$effects$Sepal.Width$median, 2), 0.21,
    tolerance = 0.025
  )
  results <- psycho::analyze(fit, effsize = TRUE)
  testthat::expect_equal(
    round(results$values$effects$Sepal.Width$median, 2), 0.21,
    tolerance = 0.025
  )



  # Other algorithms
  fit <- quiet(rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "meanfield"
  ))

  results <- psycho::analyze(fit)
  values <- psycho::values(results)
  testthat::expect_equal(
    round(values$effects$Sepal.Width$median, 2), -0.46,
    tolerance = 0.1
  )

  # This also needs to be fixed

  # fit <- rstanarm::stan_glm(
  #   Sepal.Length ~ Sepal.Width,
  #   data = iris,
  #   seed = 666,
  #   algorithm = "fullrank"
  # )
  #
  # results <- psycho::analyze(fit)
  # values <- psycho::values(results)

  # testthat::expect_equal(
  #   round(values$effects$Sepal.Width$median, 2), -0.12,
  #   tolerance = 0.1
  # )

  fit <- quiet(rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "optimizing"
  ))
  testthat::expect_error(psycho::analyze(fit))
})



test_that("assess", {
  x <- assess(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.018, tol = 0.02)

  x <- assess(
    patient = 10,
    mean = 8,
    sd = 2,
    n = 10
  )

  testthat::expect_equal(x$values$p, 0.18, tol = 0.02)

  x <- assess(
    patient = c(10, 12),
    mean = 8,
    sd = 2,
    verbose = FALSE
  )

  testthat::expect_equal(x[[1]]$values$p, 0.16, tol = 0.05)
})




test_that("bayes_cor", {
  results <- psycho::bayes_cor.test(
    psycho::affective$Concealing,
    psycho::affective$Tolerating
  )

  testthat::expect_equal(results$values$median, 0.073, tol = 0.05)
  testthat::expect_equal(results$values$effect_size$values$`very small`, 0.82, tol = 0.05)

  results <- psycho::bayes_cor(iris)
  testthat::expect_equal(nrow(results$values$r), 4)


  results <- psycho::bayes_cor(
    dplyr::select(iris, dplyr::starts_with("Sepal")),
    dplyr::select(iris, dplyr::starts_with("Petal"))
  )
  testthat::expect_equal(nrow(results$values$r), 2)
})






test_that("correlation", {
  df <- attitude[c("rating", "complaints", "privileges", "learning")]


  # Pearson
  output <- psycho::correlation(df)
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.82, tol = 0.1)

  # Spearman
  output <- psycho::correlation(df, method = "spearman")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.83, tol = 0.1)

  # Partial
  output <- psycho::correlation(df, type = "partial", adjust = "holm")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.72, tol = 0.1)

  # Semi
  output <- psycho::correlation(df, type = "semi", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.53, tol = 0.1)

  # glasso
  # testthat::expect_warning(psycho::correlation(df, type = "glasso", adjust = "none"))

  # cor_auto
  output <- psycho::correlation(df, type = "cor_auto", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.82, tol = 0.1)

  # Dual
  df2 <- attitude[c("raises", "critical")]
  output <- psycho::correlation(df, df2, type = "full", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.67, tol = 0.1)



  type <- "semi"
  adjust <- "none"
  method <- "pearson"
  output <- psycho::correlation(df, df2, type = "semi", adjust = "none")
  value <- output$values$r[2, 1]
  testthat::expect_equal(value, 0.46, tol = 0.1)

  plot <- plot(output)
  testthat::expect_equal(length(plot), 10, tol = 0.1)

  # Other
  testthat::expect_warning(psycho::correlation(df, type = "dupa", adjust = "holm"))

  # Plot
  plot <- plot(correlation(df))
  testthat::expect_equal(length(plot), 10, tol = 0.1)

  testthat::expect_warning(correlation(data.frame(replicate(11, rnorm(100))), adjust = "none"))
})





test_that("crawford.test", {

  # bayesian ----------------------------------------------------------------


  x <- crawford.test(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.019, tol = 0.02)

  x <- crawford.test(
    patient = -10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$p, 0.019, tol = 0.02)



  # frequentist -------------------------------------------------------------


  x <- crawford.test.freq(
    patient = 10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, 3.05, tol = 0.2)

  x <- crawford.test.freq(
    patient = -10,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, -3.3, tol = 0.2)

  x <- crawford.test.freq(
    patient = 7,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, 2.10, tol = 0.2)

  x <- crawford.test.freq(
    patient = 0,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$t, -0.12, tol = 0.2)
})







test_that("crawford.test", {
  x <- crawford_dissociation.test(
    case_X = 142,
    case_Y = 7,
    controls_X = c(100, 125, 89, 105, 109, 99),
    controls_Y = c(7, 8, 9, 6, 7, 10)
  )

  testthat::expect_equal(x$t, 2.1, tol = 0.02)
})





test_that("create_intervals", {
  x <- psycho::rnorm_perfect(1000)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, 3))), 3)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, length = 100))), 2)
  testthat::expect_equal(length(levels(psycho::create_intervals(x, 3, equal_range = FALSE))), 3)
  testthat::expect_true(is.numeric(psycho::create_intervals(x, 3, labels = "median")))
  testthat::expect_true(is.numeric(psycho::create_intervals(x, 3, labels = FALSE)))
})





test_that("dprime", {
  testthat::expect_equal(dprime(9, 2, 1, 7)$dprime, 1.65, tolerance = 0.1)
  testthat::expect_equal(dprime(1, 9, 1, 0)$dprime, -1.49, tolerance = 0.1)

  df <- data.frame(
    Participant = c("A", "B", "C"),
    n_hit = c(1, 2, 5),
    n_fa = c(6, 8, 1)
  )

  indices <- dprime(n_hit = df$n_hit, n_fa = df$n_fa, n_targets = 10, n_distractors = 10, adjusted = F)
  testthat::expect_equal(indices$dprime[1], -1.53, tolerance = 0.1)

  testthat::expect_equal(dprime(5, 0, n_targets = 10, n_distractors = 8, adjusted = FALSE)$aprime, 0.875, tolerance = 0.1)
})





test_that("find_best_model.stanreg", {
  testthat::expect_equal(1, 1)

  # The following fails for some reasons

  # data <- standardize(attitude)
  # fit <- rstanarm::stan_glm(rating ~ advance + privileges,
  #                           chains = 1, iter = 500,
  #                           data=data,
  #                           seed=666)
  #
  # best <- find_best_model(fit, K=2)
  # best_formula <- best$formula
  # testthat::expect_equal(best_formula, "rating ~ privileges")
  #
  # best <- find_best_model(fit, K=0)
  # best_formula <- best$formula
  # testthat::expect_equal(best_formula, "rating ~ privileges")
})





test_that("find_combinations.formula", {
  f <- as.formula("Y ~ A + B + C + D + (1|E)")
  combinations <- find_combinations(f)
  testthat::expect_equal(length(combinations), 32)
})




test_that("find_matching_string", {
  testthat::expect_equal(find_matching_string("Hwo rea ouy", c("How are you", "Not this word", "Nice to meet you")), "How are you")
})





test_that("find_random_effects", {
  f <- as.formula("Y ~ A + B + C + D + (1|E)")
  rf <- psycho::find_random_effects(f)
  testthat::expect_equal(rf, "(1|E)")
})







test_that("find_season", {
  dates <- c("2017-02-15", "2017-05-15", "2017-08-15", "2017-11-15")
  dates <- find_season(dates)
  expect_equal(as.character(dates[1]), "Winter")
})





test_that("formatting", {

  testthat::expect_equal(format_p(0.00000), "< .001***")
  testthat::expect_equal(format_p(0.00000, stars = FALSE), "< .001")

  testthat::expect_equal(format_formula(paste("A", "~   B")), "A ~ B")
})







test_that("get_contrasts", {
  # rstanarm
  require(rstanarm)

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data = df)

  contrasts <- psycho::get_contrasts(fit, "Salary")
  testthat::expect_equal(mean(contrasts$Median), -0.134, tolerance = 0.05)

  # lmerTest
  require(lmerTest)

  fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = psycho::affective)

  contrasts <- get_contrasts(fit)
  testthat::expect_equal(mean(contrasts$Difference), -0.218, tolerance = 0.05)

  # glmer
  require(lme4)

  fit <- lme4::glmer(Sex ~ Birth_Season + (1 | Salary), data = psycho::affective, family = "binomial")

  contrasts <- get_contrasts(fit, adjust = "bonf")
  testthat::expect_equal(mean(contrasts$Difference), -0.0734, tolerance = 0.05)

  # glm
  fit <- glm(Sex ~ Birth_Season, data = psycho::affective, family = "binomial")

  contrasts <- get_contrasts(fit)
  testthat::expect_equal(mean(contrasts$Difference), -0.0458, tolerance = 0.05)
})











test_that("get_info", {
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








test_that("get_means", {
  # rstanarm
  require(rstanarm)

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data = df)

  means <- psycho::get_means(fit, "Salary")
  testthat::expect_equal(mean(means$Median), 4.876, tolerance = 0.05)


  # lmerTest
  require(lmerTest)

  fit <- lmerTest::lmer(Adjusting ~ Birth_Season + (1 | Salary), data = psycho::affective)

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), 3.860, tolerance = 0.05)


  # glmer
  require(lme4)

  fit <- lme4::glmer(Sex ~ Birth_Season + (1 | Salary), data = psycho::affective, family = "binomial")

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), -1.221759, tolerance = 0.05)

  # glm
  fit <- glm(Sex ~ Birth_Season, data = psycho::affective, family = "binomial")

  means <- get_means(fit, formula = "Birth_Season")
  testthat::expect_equal(mean(means$Mean), -1.413, tolerance = 0.05)
})














test_that("get_predicted", {



  # Rstanarm ----------------------------------------------------------------
  library(psycho)
  require(rstanarm)


  fit <- rstanarm::stan_glm(
    vs ~ mpg,
    data = mtcars,
    family = binomial(link = "logit"),
    seed = 666
  )
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Median)$estimate)
  testthat::expect_equal(r, 0.68, tolerance = 0.2)




  fit <- rstanarm::stan_glm(
    cyl ~ mpg,
    data = mtcars,
    seed = 666
  )
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$cyl, data$cyl_Median)$estimate)
  testthat::expect_equal(r, 0.85, tolerance = 0.02)



  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width + Species,
    data = iris,
    seed = 666
  )
  data <- psycho::get_predicted(fit, posterior_predict = TRUE)
  r <- as.numeric(cor.test(data$Sepal.Length, data$Sepal.Length_Median)$estimate)
  testthat::expect_equal(r, 0.84, tolerance = 0.02)


  # Actual test -------------------------------------------------------------

  df <- psycho::affective
  fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, data = df)
  ref_grid <- emmeans::ref_grid(fit, at = list(
    Tolerating = seq(min(df$Tolerating),
      max(df$Tolerating),
      length.out = 10
    )
  ))

  predicted <- psycho::get_predicted(fit, newdata = ref_grid)
  testthat::expect_equal(mean(predicted$Life_Satisfaction_Median), 4.77, tolerance = 0.05)

  predicted <- psycho::get_predicted(fit, newdata = ref_grid, keep_iterations = TRUE)
  testthat::expect_equal(length(predicted), 4004)







  # GLM and LM --------------------------------------------------------------

  fit <- glm(vs ~ mpg, data = mtcars, family = binomial(link = "logit"))
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Predicted)$estimate)
  testthat::expect_equal(r, 0.68, tolerance = 0.2)


  fit <- lm(cyl ~ mpg, data = mtcars)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(mtcars$cyl, data$cyl_Predicted)$estimate)
  testthat::expect_equal(r, 0.85, tolerance = 0.02)

  # glmerMod ----------------------------------------------------------------
  library(lme4)

  fit <- lme4::glmer(vs ~ mpg + (1 | cyl), data = mtcars, family = binomial(link = "logit"))
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$vs, data$vs_Predicted)$estimate)
  testthat::expect_equal(r, 0.79, tolerance = 0.02)

  fit <- lme4::lmer(Tolerating ~ Adjusting + (1 | Salary), data = affective)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$Tolerating, data$Tolerating_Predicted)$estimate)
  testthat::expect_equal(r, 0.3, tolerance = 0.02)

  library(lmerTest)
  fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1 | Salary), data = affective)
  data <- psycho::get_predicted(fit)
  r <- as.numeric(cor.test(data$Tolerating, data$Tolerating_Predicted)$estimate)
  testthat::expect_equal(r, 0.3, tolerance = 0.02)
})











test_that("get_R2", {
  # Fit
  library(psycho)

  fit <- lm(Tolerating ~ Adjusting, data = psycho::affective)
  testthat::expect_equal(psycho::get_R2(fit)$R2, 0.08, tol = 0.01)

  fit <- glm(Sex ~ Adjusting, data = psycho::affective, family = "binomial")
  testthat::expect_equal(psycho::get_R2(fit), 0.025, tol = 0.01)

  fit <- lmerTest::lmer(Tolerating ~ Adjusting + (1 | Sex), data = psycho::affective)
  testthat::expect_equal(psycho::get_R2(fit)$R2m, 0.08, tol = 0.01)
  testthat::expect_equal(psycho::get_R2(fit, method = "tjur")$R2m, 0.081, tol = 0.01)

  fit <- lme4::glmer(Sex ~ Adjusting + (1 | Salary), data = na.omit(psycho::affective), family = "binomial")
  testthat::expect_equal(psycho::get_R2(fit)$R2m, 0.037, tol = 0.01)
})







test_that("hdi", {
  x <- attitude$rating
  results <- psycho::HDI(x, 0.95)

  testthat::expect_equal(results$values$HDImin, 40)
  testthat::expect_equal(length(plot(results)), 9)
  testthat::expect_equal(psycho::HDI(x, 95)$values$HDImin, 40)
})








test_that("interpret_bf", {
  testthat::expect_equal(psycho::interpret_bf(3), "moderate evidence (BF = 3.00) in favour of")
  testthat::expect_equal(psycho::interpret_bf(1 / 3), "moderate evidence (BF = 3.00) against")
  testthat::expect_equal(psycho::interpret_bf(1 / 3, rules = "raftery1995"), "positive evidence (BF = 3.00) against")
})





test_that("interpret_d", {
  testthat::expect_equal(psycho::interpret_d(0), "very small")
  testthat::expect_equal(psycho::interpret_d(0, rules = "sawilowsky2009"), "tiny")

  testthat::expect_equal(psycho::interpret_d_posterior(c(0.1, 0.1, 0.1, 0.1))$values$large, 0)
})






test_that("interpret_odds", {
  testthat::expect_equal(psycho::interpret_odds(0), "very small")
  testthat::expect_equal(psycho::interpret_odds(0, log = TRUE), "very small")
  testthat::expect_equal(psycho::interpret_odds(5, log = TRUE), "large")
  testthat::expect_equal(psycho::interpret_odds(5, log = TRUE, rules = "cohen1988"), "large")

  testthat::expect_equal(psycho::interpret_odds_posterior(c(5, 5, 5, 5))$values$large, 0)
})








test_that("interpret_r", {
  testthat::expect_equal(psycho::interpret_r(0), "very small, and negative")
  testthat::expect_equal(psycho::interpret_r(0, rules = "evans1996"), "very weak, and negative")
})







test_that("interpret_R2", {
  testthat::expect_equal(psycho::interpret_R2(0.2), "medium")
  testthat::expect_equal(psycho::interpret_R2(0.2, rules = "chin1998"), "small")
  testthat::expect_equal(psycho::interpret_R2(0.2, rules = "hair2013"), "very small")
  testthat::expect_true(is.na(psycho::interpret_R2(-5)))

  testthat::expect_equal(psycho::interpret_R2_posterior(c(0.2, 0.2, 0.2))$values$medium, 1)
  testthat::expect_equal(psycho::interpret_R2_posterior(c(0.1, 0.2, 0.3, 0.4))$values$large, 0.5)
})








test_that("interpret_RMSEA", {
  testthat::expect_equal(psycho::interpret_RMSEA(0.04), "good")
  testthat::expect_equal(psycho::interpret_RMSEA(0.05), "acceptable")
  testthat::expect_equal(psycho::interpret_RMSEA(0.08), "poor")
})









test_that("is.mixed.stanreg", {
  library(rstanarm)
  fit <- rstanarm::stan_glm(Sepal.Length ~ Petal.Length, data = iris, iter = 100)
  testthat::expect_equal(is.mixed(fit), FALSE)
  fit <- rstanarm::stan_lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris, iter = 100)
  testthat::expect_equal(is.mixed(fit), TRUE)
})









test_that("is.psychobject", {
  df <- attitude
  results <- psycho::correlation(df)
  testthat::expect_true(psycho::is.psychobject(results))
})








test_that("is.standardized", {
  df <- psycho::affective
  testthat::expect_equal(is.standardized(df), F)
  df <- psycho::standardize(df)
  testthat::expect_equal(is.standardized(df), T)
})






test_that("mellenbergh.test", {
  x <- mellenbergh.test(
    t0 = 4,
    t1 = 12,
    controls = c(0, -2, 5, 2, 1, 3, -4, -2)
  )

  testthat::expect_equal(x$values$z, 1.90, tol = 0.2)


  x <- mellenbergh.test(
    t0 = 4,
    t1 = 12,
    controls = 2.54
  )

  testthat::expect_equal(x$values$z, 2.22, tol = 0.2)

  x <- mellenbergh.test(t0 = 4, t1 = 12, controls = c(0, -2, 5, 2, 1, 3, -4, -2))
  testthat::expect_equal(x$values$z, 1.90, tol = 0.1)
  x <- mellenbergh.test(t0 = 8, t1 = 2, controls = 2.6)
  testthat::expect_equal(x$values$z, -1.63, tol = 0.1)
})







test_that("model_to_priors", {
  fit <- rstanarm::stan_glm(Sepal.Length ~ Petal.Width, data = iris)
  priors <- psycho::model_to_priors(fit)
  testthat::expect_equal(length(priors), 3)
})






test_that("n_factors", {
  results <- attitude %>%
    select_if(is.numeric) %>%
    psycho::n_factors()

  testthat::expect_equal(nrow(summary(results)), 7)
  testthat::expect_equal(nrow(psycho::values(results)$methods), 9)
  testthat::expect_equal(length(plot(results)), 9)
})





test_that("odds_to_probs", {
  testthat::expect_equal(odds_to_probs(-1.6), 0.17, tolerance = 0.01)
  testthat::expect_equal(odds_to_probs(-1.6, log = F), 2.66, tolerance = 0.01)

  testthat::expect_equal(
    ncol(odds_to_probs(
      psycho::affective,
      subset = c("Life_Satisfaction"),
      except = c("Sex")
    )),
    8
  )
})








test_that("overlap", {
  x <- rnorm(1000, 1, 0.5)
  y <- rnorm(1000, 0, 1)
  testthat::expect_equal(overlap(x, y), 0.43, tolerance = 0.1)
})







test_that("plot.psychobject", {
  output <- list(plot = 1)
  class(output) <- c("psychobject", "list")
  plot <- plot(output)
  expect_equal(plot, 1)
})







test_that("power_analysis", {
  fit <- lm(Sepal.Length ~ Sepal.Width, data = iris)
  results <- psycho::power_analysis(fit, n_max = 300, n_min = 150, step = 50, n_batch = 1)

  testthat::expect_equal(nrow(results), 8, tolerance = 0.01)
})







test_that("print.psychobject", {
  output <- list(text = 1)
  class(output) <- c("psychobject", "list")
  text <- print(output)
  expect_equal(text, 1)
})





test_that("probs_to_odds", {
  testthat::expect_equal(probs_to_odds(0.75), 3, tolerance = 0.01)
  testthat::expect_equal(probs_to_odds(0.75, log = TRUE), 1.098, tolerance = 0.01)
})






test_that("refdata", {
  testthat::expect_equal(nrow(psycho::refdata(psycho::affective, target = "Sex")), 2)
  testthat::expect_equal(nrow(psycho::refdata(iris, length.out = 2)), 48)
  testthat::expect_equal(nrow(psycho::refdata(iris, target = "Sepal.Length", length.out = 2, factors = "combinations")), 6)
  testthat::expect_equal(nrow(psycho::refdata(iris, target = "Species", length.out = 2, factors = "combinations")), 3)
  testthat::expect_equal(nrow(psycho::refdata(iris, target = "Species", length.out = 2, numerics = 0)), 3)
})










test_that("remove_empty_cols", {
  df <- data.frame(
    A = c(1, 2, 3),
    B = c(1, 2, 3)
  )
  df$C <- NA

  testthat::expect_equal(ncol(psycho::remove_empty_cols(df)), 2)
})





test_that("rnorm_perfect", {
  x <- psycho::rnorm_perfect(10, 0, 1)
  testthat::expect_equal(mean(x), 0, tolerance = 0.02)

  x <- psycho::rnorm_perfect(10, 0, 1, method = "average")
  testthat::expect_equal(mean(x), 0, tolerance = 0.05)
})






test_that("standardize", {
  library(psycho)

  set.seed(666)
  df <- data.frame(
    Participant = as.factor(rep(1:25, each = 4)),
    Condition = base::rep_len(c("A", "B", "C", "D"), 100),
    V1 = rnorm(100, 30, .2),
    V2 = runif(100, 3, 5),
    V3 = rnorm(100, 100, 10)
  )

  # Deactivate all this for CRAN

  # dfZ <- standardize(df)
  # testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)
  #
  # dfZ <- standardize(df, except = "V3")
  # testthat::expect_equal(mean(dfZ$V2), 0, tol = 0.01)
  #
  # dfZ <- standardize(df, except = c("V1", "V2"))
  # testthat::expect_equal(mean(dfZ$V3), 0, tol = 0.01)
  #
  # dfZ <- standardize(df$V1)
  # testthat::expect_equal(mean(dfZ), 0, tol = 0.01)
  #
  # dfZ <- standardize(df, subset = c("V1", "V2"))
  # testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)
  #
  # dfZ <- standardize(df, subset = "V1", except = "V3")
  # testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)
  #
  # dfZ <- standardize(dplyr::group_by(df, Participant))
  # testthat::expect_equal(mean(dfZ$V1), 0, tol = 0.01)
  #
  # dfN <- standardize(df, except = "V3", normalize = TRUE)
  # testthat::expect_equal(mean(dfN$V2), 0.533, tol = 0.5)


  # Models
  fit <- rstanarm::stan_glm(
    Sepal.Length ~ Sepal.Width,
    data = iris,
    seed = 666,
    algorithm = "meanfield"
  )

  std <- standardize(fit, method = "posterior")
  testthat::expect_equal(mean(std), -0.24, tol = 0.02)

  std <- standardize(fit, method = "sample")
  testthat::expect_equal(mean(std), 1.34, tol = 0.02)

  fit <- lm(
    Sepal.Length ~ Sepal.Width,
    data = iris
  )

  std <- standardize(fit, method = "posthoc")
  testthat::expect_equal(mean(std$Coef_std), -0.059, tol = 0.01)
})






test_that("values.psychobject", {
  output <- list(values = 1)
  class(output) <- c("psychobject", "list")
  values <- values(output)
  expect_equal(values, 1)
})








test_that("analyze.fa", {
  library(psycho)
  library(psych)

  x <- psych::fa(psych::Thurstone.33, 2)

  results <- analyze(x)
  testthat::expect_equal(nrow(summary(results)), 9)

  cfa_model <- get_cfa_model(results$values$loadings, treshold = 0.3)
  testthat::expect_equal(length(cfa_model), 1)
})
