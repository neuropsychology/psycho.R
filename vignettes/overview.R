## ---- echo=F, message=FALSE, warning=FALSE-------------------------------
library(knitr)
library(tidyverse)
library(broom)
library(rstanarm)

## ------------------------------------------------------------------------
# Do this once (uncomment if needed)
# install.packages("devtools") 
# library(devtools)
# devtools::install_github("https://github.com/neuropsychology/psycho.R")

# Load psycho (at the beginning of every script)
library(psycho)

## ---- out.width=800, echo = FALSE, eval = TRUE, fig.align='center'-------
knitr::include_graphics("images/workflow.PNG")

## ---- fig.width=8, eval = TRUE, fig.align='center'-----------------------
library(psycho)

df <- iris

cor <- psycho::correlation(df)

cortable <- print(cor)

# You can save it using write.csv(cortable, "correlation_table.csv")
# Or diplay it using View(cortable)

kable(cortable)


# You can also plot it
cor$plot()

## ---- out.width=8, eval = TRUE, fig.align='center'-----------------------
library(psycho)
library(tidyverse)

# Normalize all numeric variables
df <- iris %>% 
  psycho::normalize()

summary(df)

## ---- fig.width=8, eval = TRUE, fig.align='center'-----------------------
library(psycho)

results <- psycho::assess(124, mean=100, sd=15)

# Print it
print(results)

# Plot it
plot(results)

## ---- fig.width=8, eval = TRUE, fig.align='center'-----------------------
library(psycho)
library(rstanarm)

# Create dataframe
df <- data.frame(Participant = as.factor(rep(1:50,each=2)), Condition = base::rep_len(c("A", "B"), 100), V1 = rnorm(100, 30, .2), V2 = runif(100, 3, 5))
df <- psycho::normalize(df)

# Show dataframe
kable(head(df))


## ---- eval=TRUE, fig.align='center', fig.width=8, message=FALSE, results="hide"----
# Fit bayesian mixed model
fit <- rstanarm::stan_lmer(V1 ~ Condition / V2 + (1|Participant), data=df)

## ---- fig.width=8, eval = TRUE, fig.align='center'-----------------------
results <- psycho::analyze(fit)

# Print summary
kable(summary(results))

# Show text
print(results)

# Plot effects
plot(results)

