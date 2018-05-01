The data
========

Let's start by saying you did a pilot study. However, you'd like to know how many participants you should include in your final study to avoid a type II error (false negative, concluding to an absence of effect when in fact the effect exist).

An an example, let's say we are interested in the amount of memories (the *autobiographical link*) associated with negative and neutral pictures. More specifically, we are interested to see if this **autobiographical link is related to the age of the participants**. Let's start by generating the correct dataframe.

``` r
library(dplyr)
library(psycho)

df <- psycho::emotion %>% 
  group_by(Participant_ID) %>% 
  select_if(is.numeric) %>% 
  summarise_all(mean)

head(df)
```

| Participant\_ID |  Participant\_Age|  Trial\_Order|  Subjective\_Arousal|  Subjective\_Valence|  Autobiographical\_Link|
|:----------------|-----------------:|-------------:|--------------------:|--------------------:|-----------------------:|
| 10S             |          18.86927|          24.5|             67.43164|            -15.70095|               60.948351|
| 11S             |          21.30869|          24.5|             35.27635|            -35.69272|               12.252166|
| 12S             |          20.90623|          24.5|             53.28234|            -25.73785|               35.948351|
| 13S             |          18.31622|          24.5|             53.45595|            -19.02127|               48.301866|
| 14S             |          21.20465|          24.5|             40.20940|            -21.60505|                4.489995|
| 15S             |          21.62628|          24.5|             46.61458|            -29.94792|               23.030599|

Our dataframe

``` r
fit <- lm(Autobiographical_Link~Participant_Age, data=df)
results <- psycho::analyze(fit)
print(results)
```

    The overall model predicting Autobiographical_Link (formula = Autobiographical_Link ~ Participant_Age) successfully converged and explained 18.40% of the variance of the endogen (adjusted R2 = 13.30). Within this model:
       - The effect of (Intercept) is  significant (beta = 97.01, SE = 36.59, 95% CI [19.43, 174.58]), t = 2.65, p < .05*) and can be considered as very small (std. beta = 0, std. SE = 0).
       - The effect of Participant_Age is  significant (beta = -3.30, SE = 1.74, 95% CI [-6.99, 0.38]), t = -1.90, p = 0.08°) and can be considered as small (std. beta = -0.43, std. SE = 0.23).

Frequentist
===========

``` r
#' fit <- lm(Sepal.Length ~ Sepal.Width, data=iris)
#'
#' results <- power_analysis(fit, n_max=300, n_min=100, step=5, n_batch=20)
#'
#' results %>%
#'   filter(Variable=="Sepal.Width") %>%
#'   select(n, p) %>%
#'   group_by(n) %>%
#'   summarise(p_median = median(p),
#'             p_mad = mad(p)) %>%
#'   ggplot(aes(x=n, y=p_median)) +
#'   geom_hline(aes(yintercept=0.05), color="red", linetype="dashed") +
#'    geom_line() +
#'    geom_ribbon(aes(ymin=p_median-p_mad, ymax=p_median+p_mad), alpha=0.2) +
#'    geom_smooth(method="lm", formula = y ~ poly(x, 2))
```

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>
