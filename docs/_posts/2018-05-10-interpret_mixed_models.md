---
layout: post
title: "Format and Interpret Linear Mixed Models"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-05-10
summary: Format and Interpret Linear Mixed Models.
---


-   [The data](#the-data)
-   [Fit the model](#fit-the-model)
-   [The analyze function](#the-analyze-function)
-   [Summary](#summary)
-   [Print](#print)
-   [Credits](#credits)

You find it time-consuming to manually format, copy and paste output values to your report or manuscript? That time is over: the [`psycho`](https://github.com/neuropsychology/psycho.R) package is here for you!

The data
========

Let's take the example dataset included in the `psycho` package.

``` r
library(psycho)
library(tidyverse)

df <- psycho::emotion %>% 
  select(Participant_ID, 
         Emotion_Condition, 
         Subjective_Valence,
         Autobiographical_Link)

summary(df)
```

     Participant_ID Emotion_Condition Subjective_Valence Autobiographical_Link
     10S    : 48    Negative:456      Min.   :-100.000   Min.   :  0.00       
     11S    : 48    Neutral :456      1st Qu.: -65.104   1st Qu.:  0.00       
     12S    : 48                      Median :  -2.604   Median : 16.15       
     13S    : 48                      Mean   : -18.900   Mean   : 28.99       
     14S    : 48                      3rd Qu.:   7.000   3rd Qu.: 59.90       
     15S    : 48                      Max.   : 100.000   Max.   :100.00       
     (Other):624                                         NA's   :1            

Our dataframe (called `df`) contains data from several participants, exposed to neutral and negative pictures (the `Emotion_Condition` column). Each row corresponds to a single trial. During each trial, the participant had to rate its emotional valence (`Subjective_Valence`: positive - negative) experienced during the picture presentation and the amount of personal memories associated with the picture (`Autobiographical_Link`).

Our dataframe contains, for each of the 48 trials, 4 variables: the **name of the participant** (`Participant_ID`), the **emotion condition** (`Emotion_Condition`), the **valence rating** (`Subjective_Valence`) and the **Autobiographical Link** (`Autobiographical_Link`).

Fit the model
=============

**Let's fit a linear mixed model to predict the autobiographical link with the condition and the subjective valence.**

``` r
library(lmerTest)
fit <- lmer(Autobiographical_Link ~ Emotion_Condition * Subjective_Valence + (1|Participant_ID), data=df)
summary(fit)
```

    Linear mixed model fit by REML. t-tests use Satterthwaite's method [
    lmerModLmerTest]
    Formula: Autobiographical_Link ~ Emotion_Condition * Subjective_Valence +  
        (1 | Participant_ID)
       Data: df

    REML criterion at convergence: 8555.5

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.2682 -0.6696 -0.2371  0.7052  3.2187 

    Random effects:
     Groups         Name        Variance Std.Dev.
     Participant_ID (Intercept) 243.1    15.59   
     Residual                   661.4    25.72   
    Number of obs: 911, groups:  Participant_ID, 19

    Fixed effects:
                                                 Estimate Std. Error        df
    (Intercept)                                  25.52248    4.23991  31.49944
    Emotion_ConditionNeutral                      6.13715    2.66993 895.13045
    Subjective_Valence                            0.05772    0.03430 898.46616
    Emotion_ConditionNeutral:Subjective_Valence   0.16140    0.05020 896.26695
                                                t value Pr(>|t|)    
    (Intercept)                                   6.020 1.09e-06 ***
    Emotion_ConditionNeutral                      2.299  0.02176 *  
    Subjective_Valence                            1.683  0.09280 .  
    Emotion_ConditionNeutral:Subjective_Valence   3.215  0.00135 ** 
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Correlation of Fixed Effects:
                (Intr) Emt_CN Sbjc_V
    Emtn_CndtnN -0.459              
    Sbjctv_Vlnc  0.455 -0.726       
    Emtn_CN:S_V -0.308  0.301 -0.676

The analyze function
====================

The `analyze` function, available in the `psycho` package, transforms a model fit object into user-friendly outputs.

``` r
results <- analyze(fit, CI = 95)
```

Summary
=======

Summarizing an analyzed object returns a dataframe, that can be easily saved and included in reports. It also includes standardized coefficients, as well as bootstrapped confidence intervals (CI) and effect sizes.

``` r
summary(results) %>% 
  mutate(p = psycho::format_p(p))
```

| Variable                                      |   Coef|    SE|     t|      df|  Coef.std|  SE.std| p               | Effect\_Size |  CI\_lower|  CI\_higher|
|:----------------------------------------------|------:|-----:|-----:|-------:|---------:|-------:|:----------------|:-------------|----------:|-----------:|
| (Intercept)                                   |  25.52|  4.24|  6.02|   31.50|      0.00|    0.00| &lt; .001\*\*\* | Very Small   |      17.16|       33.93|
| Emotion\_ConditionNeutral                     |   6.14|  2.67|  2.30|  895.13|      0.10|    0.04| &lt; .05\*      | Very Small   |       0.91|       11.37|
| Subjective\_Valence                           |   0.06|  0.03|  1.68|  898.47|      0.09|    0.06| = 0.09°         | Very Small   |      -0.01|        0.12|
| Emotion\_ConditionNeutral:Subjective\_Valence |   0.16|  0.05|  3.22|  896.27|      0.13|    0.04| &lt; .01\*\*    | Very Small   |       0.06|        0.26|

Print
=====

Moreover, the `print` method return a nicely formatted output that can be almost directly pasted into the manuscript.

``` r
print(results)
```

    The overall model predicting Autobiographical_Link (formula = Autobiographical_Link ~ Emotion_Condition * Subjective_Valence + (1 | Participant_ID)) successfully converged and explained 32.48% of the variance of the endogen (the conditional R2). The variance explained by the fixed effects was of 7.66% (the marginal R2) and the one explained by the random effects of 24.82%. The model's intercept is at 25.52 (SE = 4.24, 95% CI [17.16, 33.93]). Within this model:
       - The effect of Emotion_ConditionNeutral is significant (beta = 6.14, SE = 2.67, 95% CI [0.91, 11.37], t(895.13) = 2.30, p < .05*) and can be considered as very small (std. beta = 0.098, std. SE = 0.043).
       - The effect of Subjective_Valence is significant (beta = 0.058, SE = 0.034, 95% CI [-0.0097, 0.12], t(898.47) = 1.68, p = 0.09°) and can be considered as very small (std. beta = 0.095, std. SE = 0.056).
       - The effect of Emotion_ConditionNeutral:Subjective_Valence is significant (beta = 0.16, SE = 0.050, 95% CI [0.063, 0.26], t(896.27) = 3.22, p < .01**) and can be considered as very small (std. beta = 0.13, std. SE = 0.041).

The intercept (the baseline level) corresponds, here, to the negative condition with subjective valence at 0 (the average, as the data is standardized). Compared to that, changing the condition from negative to neutral does not induce any significant change to the outcome. However, in the negative condition, there is a trending linear relationship between valence and autobiographical memories: the more an item is positive the more it is related to memories. Finally, the interaction is significant: the relationship between valence autobiographical memories is stronger (more positive) in the neutral condition.

Credits
=======

This package helped you? You can cite [`psycho`](https://github.com/neuropsychology/psycho.R) as follows:

-   Makowski, (2018). *The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>



Previous blogposts
==================

-   [Copy/paste t-tests Directly to Manuscripts](https://neuropsychology.github.io/psycho.R/2018/06/19/analyze_ttest.html)
-   [APA Formatted Bayesian Correlation](https://neuropsychology.github.io/psycho.R/2018/06/11/bayesian_correlation.html)
-   [Fancy Plot (with Posterior Samples) for Bayesian Regressions](https://neuropsychology.github.io/psycho.R/2018/06/03/plot_bayesian_model.html)
-   [How Many Factors to Retain in Factor Analysis](https://neuropsychology.github.io/psycho.R/2018/05/24/n_factors.html)
-   [Beautiful and Powerful Correlation Tables](https://neuropsychology.github.io/psycho.R/2018/05/20/correlation.html)
-   [Format and Interpret Linear Mixed Models](https://neuropsychology.github.io/psycho.R/2018/05/10/interpret_mixed_models.html)
-   [How to do Repeated Measures ANOVAs](https://neuropsychology.github.io/psycho.R/2018/05/01/repeated_measure_anovas.html)
-   [Standardize (Z-score) a dataframe](https://neuropsychology.github.io/psycho.R/2018/03/29/standardize.html)
-   [Compute Signal Detection Theory Indices](https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html)
