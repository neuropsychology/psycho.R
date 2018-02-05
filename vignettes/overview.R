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

## ---- out.width=700, echo = FALSE, eval = TRUE, fig.align='center'-------
knitr::include_graphics("images/workflow.PNG")

## ---- fig.width=8, eval = TRUE, fig.align='center', results='hide'-------
library(psycho)

df <- iris

cor <- psycho::correlation(df, 
                           type = "full",
                           method = "pearson",
                           adjust = "none")

summary(cor)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(cor))

## ---- fig.width=8, eval = TRUE, fig.align='center'-----------------------
plot(cor)

## ---- fig.width=8, eval = TRUE, fig.align='center', results='markup', comment=NA----
library(psycho)

df <- iris

pcor <- psycho::correlation(df, 
                           type = "partial",
                           method = "pearson",
                           adjust = "bonferroni")

summary(pcor)

## ---- results='markup', comment=NA---------------------------------------
print(pcor)

## ---- out.width=8, eval = TRUE, fig.align='center', results='markup', comment=NA----
library(psycho)
library(tidyverse)

iris %>% 
  select(Species, Sepal.Length, Petal.Length) %>% 
  psycho::standardize() %>% 
  summary()

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center', comment=NA----
library(psycho)

results <- psycho::assess(124, mean=100, sd=15)

# Print it
print(results)

# Plot it
plot(results)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center'----
library(psycho)

results <- psycho::assess(85, mean=100, sd=15, linecolor = "orange", fillcolor = "#4CAF50")

# Plot it
plot(results)

## ----eval=TRUE, fig.align='center', fig.height=4.5, fig.width=9, message=FALSE, warning=FALSE, results='markup'----
library(psycho)

# Let's create a correlation plot
p <- plot(psycho::correlation(iris))

# Custom theme and colours
p <- p + 
  scale_fill_gradientn(colors = c("#4CAF50", "#FFEB3B", "#FF5722")) +
  ylab("Variables\n") +
  labs(fill = "r") +
  theme(plot.background = element_rect(fill = "#607D8B"),
        axis.title.y = element_text(size = 20, angle = 90, colour="white"),
        axis.text = element_text(size = 15, colour="white"),
        legend.title = element_text(size = 20, colour="white"),
        legend.text = element_text(size = 15, colour="white"),
        title = element_text(size = 16, colour="white"))
p


## ----echo=TRUE, message=FALSE, warning=FALSE, results='hide'-------------
results <- attitude %>%
  select_if(is.numeric) %>% 
  psycho::n_factors()

# Get a summary
summary(results)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results))

## ----echo=FALSE, message=FALSE, warning=FALSE, results='hide'------------
psycho::values(results)$methods

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(psycho::values(results)$methods)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center'----
plot(results)

## ---- results='hide'-----------------------------------------------------
set.seed(666)
df <- data.frame(Participant = as.factor(rep(1:25, each = 4)), 
                 Item = rep_len(c("i1", "i2", "i3", "i4"), 100), 
                 Condition = rep_len(c("A", "B", "A", "B", "B"), 20), 
                 Error = as.factor(sample(c(0, 1), 100, replace = T)),
                 RT = rnorm(100, 30, .2), 
                 Stress = runif(100, 3, 5))

# Standardize the numeric variables.
df <- psycho::standardize(df)

# Take a look  at the first 10 rows
head(df)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(head(df))

## ----message=FALSE, warning=FALSE, results='markup', comment=NA----------
# Format data
df_for_anova <- df %>% 
  dplyr::group_by(Participant, Condition) %>% 
  dplyr::summarise(RT = mean(RT))

# Run the anova
anova <- aov(RT ~ Condition + Error(Participant), df_for_anova)
summary(anova)

## ----fig.align='center', message=FALSE, warning=FALSE, val=TRUE, results='markup', comment=NA----
library(lme4)

fit <- lme4::lmer(RT ~ Condition + (1|Participant) + (1|Item), data=df)

# Traditional output
summary(fit)

## ---- message=FALSE, results="hide"--------------------------------------
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ---- results='markup', comment=NA---------------------------------------
print(results)

## ----fig.align='center', message=FALSE, warning=FALSE, val=TRUE, results='hide'----
library(rstanarm)

fit <- rstanarm::stan_lmer(RT ~ Condition + (1|Participant) + (1|Item), data=df)

# Traditional output
results <- psycho::analyze(fit, effsize=T)
summary(results, round=2)

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
kable(summary(results, round = 2))

## ---- results='markup', comment=NA---------------------------------------
print(results)

## ---- fig.width=7, fig.height=4.5, eval = TRUE, results='markup', fig.align='center'----
plot(results)

