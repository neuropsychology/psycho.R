## ---- echo=F, message=FALSE, warning=FALSE-------------------------------
library(knitr)

## ---- echo=T, message=FALSE, warning=FALSE, results='hide'---------------
library(rstanarm)
library(emmeans)
library(dplyr)
library(ggplot2)
library(coda)
library(psycho)

df <- psycho::affective
summary(df)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
summary(df)

## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, data=df)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
print(results)

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
# We enter the values of tolerating that we want in our reference grid, in this case a sequence of length=10 with minimum and maximum similar to the actual data.
ref_grid <- emmeans::ref_grid(fit, at = list(
  Tolerating = seq(min(df$Tolerating),
                   max(df$Tolerating),
                   length.out = 10)))
                              
predicted <- psycho::get_predicted(fit, refgrid=ref_grid)


## ----echo=T, message=FALSE, warning=FALSE, results='hide'----------------
predicted

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(predicted)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Tolerating, y=pred_Life_Satisfaction)) +
  geom_line() +
  geom_ribbon(aes(ymin=`pred_Life_Satisfaction_5%`, 
                  ymax=`pred_Life_Satisfaction_95%`), 
              alpha=0.1)


## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data=df)

## ---- message=FALSE, warning=FALSE---------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
print(results)

## ---- message=FALSE, results="hide"--------------------------------------
contrasts <- psycho::get_contrasts(fit, "Salary")

## ----echo=T, message=FALSE, warning=FALSE, results='hide'----------------
contrasts$means

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(contrasts$means, digits=2)

## ----echo=T, message=FALSE, warning=FALSE, results='hide'----------------
contrasts$contrasts

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(contrasts$contrasts, digits=2)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(contrasts$means, aes(x=Contrast, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Life Satisfaction") +
  xlab("Salary")

## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = binomial(link="logit"))

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
levels(df$Sex)

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
ref_grid <- emmeans::ref_grid(fit, at = list(
  Adjusting = seq(min(df$Adjusting),
                   max(df$Adjusting),
                   length.out = 100)))

predicted <- psycho::get_predicted(fit, refgrid=ref_grid)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
predicted$pred_Sex_proba <- psycho::odds_to_probs(predicted$pred_Sex)
predicted$`pred_Sex_5%_proba` <- psycho::odds_to_probs(predicted$`pred_Sex_5%`)
predicted$`pred_Sex_95%_proba` <- psycho::odds_to_probs(predicted$`pred_Sex_95%`)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Adjusting, y=pred_Sex_proba)) +
  geom_line() +
  geom_ribbon(aes(ymin=`pred_Sex_5%_proba`, 
                  ymax=`pred_Sex_95%_proba`), 
              alpha=0.1) +
  ylab("Probability of being a male")

## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Concealing * Sex, data=df)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE, results="hide"----------------
ref_grid <- emmeans::ref_grid(fit, at = list(
  Concealing = seq(min(df$Concealing),
                   max(df$Concealing),
                   length.out = 5)))
predicted <- psycho::get_predicted(fit, refgrid=ref_grid)
predicted

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(predicted)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Concealing, y=pred_Life_Satisfaction, colour=Sex, fill=Sex)) +
  geom_line() +
  geom_ribbon(aes(ymin=`pred_Life_Satisfaction_5%`, 
                  ymax=`pred_Life_Satisfaction_95%`), 
              alpha=0.1)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
ref_grid <- emmeans::ref_grid(fit, at = list(
  Concealing = seq(min(df$Concealing),
                   max(df$Concealing),
                   length.out = 100)))  # Increase this number to get more observations
predicted <- psycho::get_predicted(fit, refgrid=ref_grid)


ggplot(predicted, aes(x=Concealing, y=pred_Life_Satisfaction)) +
  geom_line(aes(colour=Sex), size=2) +
  geom_ribbon(aes(ymin=`pred_Life_Satisfaction_5%`, 
                  ymax=`pred_Life_Satisfaction_95%`,
                  fill=Sex), 
              alpha=0.2) +
  ylab("Life Satisfaction")

## ---- message=FALSE, results="hide", warning=FALSE-----------------------
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ Age + (1|Salary), data=df)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
ref_grid <- emmeans::ref_grid(fit, at = list(
  Age = seq(min(df$Age),
            max(df$Age),
            length.out = 10)))
# We name the predicted dataframe by adding _linear to keep it for further comparison (see next part)
predicted_linear <- psycho::get_predicted(fit, refgrid=ref_grid)


## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted_linear, aes(x=Age, y=pred_Concealing)) +
  geom_line() +
  geom_ribbon(aes(ymin=`pred_Concealing_5%`, 
                  ymax=`pred_Concealing_95%`), 
              alpha=0.1)

## ---- message=FALSE, results="hide", warning=FALSE-----------------------
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ poly(Age, 2, raw=TRUE) + (1|Salary), data=df)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
ref_grid <- emmeans::ref_grid(fit, at = list(
  Age = seq(min(df$Age),
            max(df$Age),
            length.out = 100)))

predicted_poly <- psycho::get_predicted(fit, refgrid=ref_grid)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted_poly, aes(x=Age, y=pred_Concealing)) +
  geom_line() +
  geom_ribbon(aes(ymin=`pred_Concealing_5%`, 
                  ymax=`pred_Concealing_95%`), 
              alpha=0.1)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
p <- ggplot() +
  # Linear model
  geom_line(data=predicted_linear, 
            aes(x=Age, y=pred_Concealing),
            colour="blue",
            size=1) +
  geom_ribbon(data=predicted_linear, 
              aes(x=Age,
                  ymin=`pred_Concealing_5%`,
                  ymax=`pred_Concealing_95%`), 
              alpha=0.1,
              fill="blue") +
  # Polynormial Model
  geom_line(data=predicted_poly, 
            aes(x=Age, y=pred_Concealing),
            colour="red",
            size=1) +
  geom_ribbon(data=predicted_poly, 
              aes(x=Age,
                  ymin=`pred_Concealing_5%`, 
                  ymax=`pred_Concealing_95%`), 
              fill="red",
              alpha=0.1) +
  # Actual data
  geom_point(data=df, aes(x=Age, y=Concealing))

library(plotly) # To create interactive plots
ggplotly(p) # To transform a ggplot into an interactive plot

## ---- message=FALSE, results="hide"--------------------------------------
# Standardize (scale and center) the numeric variables
dfZ <- psycho::standardize(df)

## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, 
                          data=dfZ,
                          prior=normal(location = 0, # Mean
                                       scale = 1, # SD
                                       autoscale=FALSE)) # Don't adjust scale automatically

## ---- message=FALSE, results="hide"--------------------------------------
results <- psycho::analyze(fit)

# Extract the posterior
posterior <- results$values$Tolerating$posterior

# Create a posterior with the prior and posterior distribution and plot them.
data.frame(posterior = posterior,
           prior = rnorm(length(posterior), 0, 1)) %>% 
  ggplot() +
  geom_density(aes(x=posterior), fill="lightblue", alpha=0.5) +
  geom_density(aes(x=prior), fill="blue", alpha=0.5) +
  scale_y_sqrt() # Change the Y axis so the plot is less ugly

