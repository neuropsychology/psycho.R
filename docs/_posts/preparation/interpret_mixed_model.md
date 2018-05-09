-   [Fit a model](#fit-a-model)
-   [The analyze function](#the-analyze-function)
-   [Summary](#summary)
-   [Print](#print)
-   [Credits](#credits)

You find it time consuming to manually format, copy and paste output values to your report or manuscript? Fear not, the `psycho` package is here for you!

Fit a model
===========

Let's take the example dataset included in the `psycho` package.

``` r
library(psycho)
library(tidyverse)

df <- psycho::emotion %>% 
  select(Participant_ID, 
         Emotion_Condition, 
         Subjective_Valence,
         Autobiographical_Link) %>% 
  standardize()

summary(df)
```

     Participant_ID Emotion_Condition Subjective_Valence Autobiographical_Link
     10S    : 48    Negative:456      Min.   :-1.5725    Min.   :-0.9235      
     11S    : 48    Neutral :456      1st Qu.:-0.8959    1st Qu.:-0.9235      
     12S    : 48                      Median : 0.3160    Median :-0.4091      
     13S    : 48                      Mean   : 0.0000    Mean   : 0.0000      
     14S    : 48                      3rd Qu.: 0.5022    3rd Qu.: 0.9847      
     15S    : 48                      Max.   : 2.3054    Max.   : 2.2624      
     (Other):624                                         NA's   :1            

Our dataframe (called `df`) contains data from several participants, exposed to neutral and negative pictures (the `Emotion_Condition` column). Each row corresponds to a single trial. As there were 48 trials per participants, there are 48 rows by participant. During each trial, the participant had to rate its emotional valence (`Subjective_Valence`: positive - negative) experienced during the picture presentation and the amount of personal memories associated with the picture (`Autobiographical_Link`).

Our dataframe contains, for each trial, 4 variables: the **name of the participant** (`Participant_ID`), the **emotion condition** (`Emotion_Condition`), the **valence rating** (`Subjective_Valence`) and the **Autobiographical Link** (`Autobiographical_Link`).

Let's fit a linear mixed model to predict the autobiographical link with the condition and the subjective valence.

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

    REML criterion at convergence: 2287.8

    Scaled residuals: 
        Min      1Q  Median      3Q     Max 
    -2.2682 -0.6696 -0.2371  0.7052  3.2187 

    Random effects:
     Groups         Name        Variance Std.Dev.
     Participant_ID (Intercept) 0.2468   0.4968  
     Residual                   0.6712   0.8193  
    Number of obs: 911, groups:  Participant_ID, 19

    Fixed effects:
                                                 Estimate Std. Error        df
    (Intercept)                                  -0.14510    0.12701  24.70753
    Emotion_ConditionNeutral                      0.09834    0.08124 894.28540
    Subjective_Valence                            0.09484    0.05636 898.46616
    Emotion_ConditionNeutral:Subjective_Valence   0.26519    0.08248 896.26695
                                                t value Pr(>|t|)   
    (Intercept)                                  -1.142  0.26422   
    Emotion_ConditionNeutral                      1.210  0.22645   
    Subjective_Valence                            1.683  0.09280 . 
    Emotion_ConditionNeutral:Subjective_Valence   3.215  0.00135 **
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    Correlation of Fixed Effects:
                (Intr) Emt_CN Sbjc_V
    Emtn_CndtnN -0.306              
    Sbjctv_Vlnc  0.322 -0.508       
    Emtn_CN:S_V -0.217 -0.057 -0.676

The analyze function
====================

The `analyze` function, available in the `psycho` package, transform a fit object into several objects. As the data are standardized, we can also compute effect sizes.

``` r
results <- analyze(fit, effsize=TRUE)
```

Summary
=======

Summarizing an analyzed object returns a dataframe, that can be easily saved and included in reports.

``` r
# We have to provide the model (here called fit and the formula of the factors we want to contrast
summary(results) %>% 
  mutate(p = psycho::format_p(p))
```

| Variable                                      |   Coef|    SE|      t|      df|  Coef.std|  SE.std| p            | Effect\_Size |  CI\_lower|  CI\_higher|
|:----------------------------------------------|------:|-----:|------:|-------:|---------:|-------:|:-------------|:-------------|----------:|-----------:|
| (Intercept)                                   |  -0.15|  0.13|  -1.14|   24.71|      0.00|    0.00| &gt; .1      | Very Small   |      -0.40|        0.11|
| Emotion\_ConditionNeutral                     |   0.10|  0.08|   1.21|  894.29|      0.05|    0.04| &gt; .1      | Very Small   |      -0.06|        0.26|
| Subjective\_Valence                           |   0.09|  0.06|   1.68|  898.47|      0.09|    0.06| = 0.09°      | Very Small   |      -0.02|        0.20|
| Emotion\_ConditionNeutral:Subjective\_Valence |   0.27|  0.08|   3.22|  896.27|      0.16|    0.05| &lt; .01\*\* | Very Small   |       0.10|        0.43|

Print
=====

Moreover, the `print` method return a nicely formatted output that can almost directly be pasted into the manuscript.

``` r
print(results)
```

    The overall model predicting Autobiographical_Link (formula = Autobiographical_Link ~ Emotion_Condition * Subjective_Valence +     (1 | Participant_ID)) successfully converged and explained 32.48% of the variance of the endogen (the conditional R2). The variance explained by the fixed effects was of 7.66% (the marginal R2) and the one explained by the random effects of 24.82%. The model's intercept is at -0.15 (SE = 0.13, 95% CI [-0.40, 0.11]). Within this model:
       - The effect of Emotion_ConditionNeutral is not significant (beta = 0.098, SE = 0.081, 95% CI [-0.061, 0.26], t(894.29) = 1.21, p > .1) and can be considered as very small (std. beta = 0.049, std. SE = 0.041).
       - The effect of Subjective_Valence is  significant (beta = 0.095, SE = 0.056, 95% CI [-0.016, 0.20], t(898.47) = 1.68, p = 0.09°) and can be considered as very small (std. beta = 0.095, std. SE = 0.056).
       - The effect of Emotion_ConditionNeutral:Subjective_Valence is  significant (beta = 0.27, SE = 0.082, 95% CI [0.10, 0.43], t(896.27) = 3.22, p < .01**) and can be considered as very small (std. beta = 0.16, std. SE = 0.049).

The intercept (the baseline level) corresponds, here, to the negative condition with subjective valence at 0 (the average, as the data is standardized). Compared to that, changing the condition from negative to neutral does not induce any significant change to the outcome. However, in the negative condition, there is a trending linear relationship between valence and autobiographical memories: the more an item is positive the more it is related to memories. Finally, the interaction is significant: the relationship between valence autobiographical memories is stronger (more positive) in the neutral condition.

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>
