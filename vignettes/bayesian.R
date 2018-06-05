## ---- echo=F, message=FALSE, warning=FALSE-------------------------------
library(knitr)
library(rstanarm)
library(emmeans)
library(dplyr)
library(tidyr)
library(ggplot2)
library(psycho)
options(mc.cores=1)

## ----message=FALSE, warning=FALSE, include=FALSE-------------------------
X <- psycho::standardize(psycho::affective$Concealing)
Y <- psycho::standardize(psycho::affective$Life_Satisfaction)
r <- cor.test(X, Y)$estimate
p <- cor.test(X, Y)$p.value
fit <- rstanarm::stan_glm(Y ~ X, seed=666, data=data.frame(Y,X))
values <- values(analyze(fit))
posterior <- values$effects$X$posterior
density <- density(posterior, n = length(posterior))
hdi <- hdi(posterior, 0.90)
mpe <- mpe(posterior)$MPE

## ----echo=FALSE, message=FALSE, warning=FALSE, fig.width=7, fig.height=4.5, fig.align='center', fig.cap="Posterior probability distribution of the correlation between X and Y"----
ggplot(data.frame(x = density$x, y = density$y), aes(x=x, y=y)) +
  xlab("\nPosterior Distribution of Correlation") + 
  ylab("Density") +
  annotate("rect", xmin = hdi$values$HDImin, xmax=hdi$values$HDImax, ymin = 0, ymax=round(max(density$y)), fill="#2196F3", alpha = .5) +
  geom_segment(aes(xend = x, yend = 0, colour = x), alpha=0.8) + 
  scale_color_gradientn(colours = c("#E91E63", "#E91E63", "#4CAF50", "#4CAF50"),
                         values=c(0, 0.4999, 0.5, 1),
                         guide=F,
                         limits = c(-1.5, 1.5)) +
  # r
  geom_vline(xintercept=r, color="#4CAF50") +
  annotate("segment", x = r+0.05, xend = r, y = 10, yend = 10, size=0.1, arrow=arrow(type="closed", length = unit(0.10, "inches")), color="#4CAF50") +
  annotate("text", x = r+0.055, y = 10, label = "Frequentist r Coefficient" , size=4 , fontface="bold", hjust = 0, color="#4CAF50") +
  # median
  geom_vline(xintercept=median(posterior)) +
  annotate("segment", x = median(posterior)+0.05, xend = median(posterior), y = 8, yend = 8, colour = "black", size=0.1, arrow=arrow(type="closed", length = unit(0.10, "inches"))) +
  annotate("text", x = median(posterior)+0.055, y = 8, label = "Posterior's Median" , size=4 , fontface="bold", hjust = 0) +
  # # mean
  # geom_vline(xintercept=mean(posterior), color="#2196F3") +
  # annotate("segment", x = mean(posterior)+0.03, xend = r, y = 6, yend = 6, size=0.1, arrow=arrow(type="closed", length = unit(0.10, "inches")), color="#2196F3") +
  # annotate("text", x = mean(posterior)+0.035, y = 6, label = "mean" , size=4 , fontface="bold", hjust = 0, color="#2196F3")
  annotate("segment", x = hdi$values$HDImin, xend = hdi$values$HDImax, y = 3, yend = 3, size=0.3, arrow=arrow(type="closed", ends="both", length = unit(0.10, "inches")), color="#2196F3") +
  annotate("text", x = -0.01, y = 3, label = "90% Credible Interval" , size=4 , fontface="bold", hjust = 0, color="#2196F3")

## ---- echo=T, message=FALSE, warning=FALSE, results='hide'---------------
library(rstanarm)
library(dplyr)
library(ggplot2)
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
knitr::kable(summary(results, round = 2))

## ----echo=TRUE, message=FALSE, warning=FALSE-----------------------------
print(results)

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
refgrid <- df %>% 
  select(Tolerating) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit, newdata=refgrid)

## ----echo=T, message=FALSE, warning=FALSE, results='hide'----------------
predicted

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(predicted)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Tolerating, y=Life_Satisfaction_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Life_Satisfaction_CI_5, 
                  ymax=Life_Satisfaction_CI_95), 
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
ggplot(contrasts$means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Life Satisfaction") +
  xlab("Salary")

## ---- message=FALSE, results="hide"--------------------------------------
# Let's fit our model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = "binomial")

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
knitr::kable(summary(results, round = 2))

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
levels(df$Sex)

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
refgrid <- df %>% 
  select(Adjusting) %>% 
  psycho::refdata(length.out=10)
  
predicted <- psycho::get_predicted(fit, newdata=refgrid)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Adjusting, y=Sex_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Sex_CI_5, 
                  ymax=Sex_CI_95), 
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
refgrid <- df %>% 
  select(Concealing, Sex) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit, newdata=refgrid)
predicted

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(predicted)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted, aes(x=Concealing, y=Life_Satisfaction_Median, fill=Sex)) +
  geom_line(aes(colour=Sex)) +
  geom_ribbon(aes(fill=Sex,
                  ymin=Life_Satisfaction_CI_5, 
                  ymax=Life_Satisfaction_CI_95), 
              alpha=0.1) +
  ylab("Life Satisfaction")

## ----eval=FALSE, message=FALSE, warning=FALSE, eval=FALSE----------------
#  # Let's fit our model (it takes more time)
#  fit <- rstanarm::stan_lmer(Concealing ~ Age + (1|Salary), data=df)

## ----message=FALSE, warning=FALSE, include=FALSE, results="hide"---------
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ Age + (1|Salary), data=df, iter=500, chains=2, seed=666)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
refgrid <- df %>% 
  select(Age) %>% 
  psycho::refdata(length.out=10)

# We name the predicted dataframe by adding '_linear' to keep it for further comparison (see next part)
predicted_linear <- psycho::get_predicted(fit, newdata=refgrid)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted_linear, aes(x=Age, y=Concealing_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Concealing_CI_5, 
                  ymax=Concealing_CI_95), 
              alpha=0.1)

## ---- message=FALSE, results="hide", warning=FALSE, eval=FALSE-----------
#  # Let's fit our model (it takes more time)
#  fit <- rstanarm::stan_lmer(Concealing ~ poly(Age, 2, raw=TRUE) + (1|Salary), data=df)

## ----message=FALSE, warning=FALSE, include=FALSE, results="hide"---------
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ poly(Age, 2, raw=TRUE) + (1|Salary), data=df, iter=500, chains=2)

## ---- message=FALSE, results="hide"--------------------------------------
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ----echo=T, message=FALSE, warning=FALSE--------------------------------
refgrid <- df %>% 
  select(Age) %>% 
  psycho::refdata(length.out=20)

predicted_poly <- psycho::get_predicted(fit, newdata=refgrid)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
ggplot(predicted_poly, aes(x=Age, y=Concealing_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Concealing_CI_5, 
                  ymax=Concealing_CI_95), 
              alpha=0.1)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
p <- ggplot() +
  # Linear model
  geom_line(data=predicted_linear, 
            aes(x=Age, y=Concealing_Median),
            colour="blue",
            size=1) +
  geom_ribbon(data=predicted_linear, 
              aes(x=Age,
                  ymin=Concealing_CI_5,
                  ymax=Concealing_CI_95), 
              alpha=0.1,
              fill="blue") +
  # Polynormial Model
  geom_line(data=predicted_poly, 
            aes(x=Age, y=Concealing_Median),
            colour="red",
            size=1) +
  geom_ribbon(data=predicted_poly, 
              aes(x=Age,
                  ymin=Concealing_CI_5, 
                  ymax=Concealing_CI_95), 
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
posterior <- results$values$effects$Tolerating$posterior

# Create a posterior with the prior and posterior distribution and plot them.
data.frame(posterior = posterior,
           prior = rnorm(length(posterior), 0, 1)) %>% 
  ggplot() +
  geom_density(aes(x=posterior), fill="lightblue", alpha=0.5) +
  geom_density(aes(x=prior), fill="blue", alpha=0.5) +
  scale_y_sqrt() # Change the Y axis so the plot is less ugly

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='hide', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
# Fit the model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = "binomial")

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA, message=FALSE, warning=FALSE----
# Generate a new refgrid
refgrid <- df %>% 
  select(Adjusting) %>% 
  psycho::refdata(length.out=10)

# Get predictions and keep iterations
predicted <- psycho::get_predicted(fit, newdata=refgrid, keep_iterations=TRUE)

# Reshape this dataframe to have iterations as factor
predicted <- predicted %>% 
  tidyr::gather(Iteration, Iteration_Value, starts_with("iter"))

# Plot iterations as well as the median prediction
ggplot(predicted, aes(x=Adjusting)) +
  geom_line(aes(y=Iteration_Value, group=Iteration), size=0.3, alpha=0.01) +
  geom_line(aes(y=Sex_Median), size=1) + 
  ylab("Male Probability\n")

