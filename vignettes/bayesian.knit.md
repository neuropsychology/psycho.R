---
title: "Bayesian Analysis in Psychology"
output: 
  rmarkdown::html_vignette:
    toc: true
author: 
- Dominique Makowski
date: "2018-06-05"
tags: [r, psychology, neuroscience]
abstract: |
  Why use frequentist methods when you can use, in an even simpler way, the Bayesian framework? Throughout this tutorial, we will explore many of the analyses you might want to do with your data.
vignette: >
  %\VignetteIndexEntry{BayesianPsychology}
  %\VignetteDepends{dplyr}
  %\VignetteDepends{tidyr}
  %\VignetteDepends{ggplot2}
  %\VignetteDepends{plotly}
  \usepackage[utf8]{inputenc}
  %\VignetteEngine{knitr::rmarkdown}
editor_options: 
  chunk_output_type: console
---



------




## The Bayesian Framework

### Why use the Bayesian Framework?

In short, because it's:

- Better
- Simpler
- Superior
- Preferable
- More appropriate
- More desirable
- More useful
- More valuable

##### **From Makowski et al. (*under review*):**

> Reasons to prefer this approach are reliability, better accuracy in noisy data, better estimation for small samples, less prone to type I error, the possibility of introducing prior knowledge into the analysis and, critically, results intuitiveness and their straightforward interpretation (Andrews & Baguley, 2013; Etz & Vandekerckhove, 2016; Kruschke, 2010; Kruschke, Aguinis, & Joo, 2012; Wagenmakers et al., 2018). Indeed, in the frequentist view, the effects are fixed (but unknown) and data are random, while the Bayesian inference calculates the probability of different effect values (called the **"posterior" distribution**) given the observed data. Bayesian’s uncertainty can be summarized, for example, by giving a range of values on the posterior distribution that includes 95% of the probability (the 95% *Credible Interval*). To illustrate the difference, the Bayesian framework allows to say "*given the observed data, the effect has 95% probability of falling within this range*", while the Frequentist less straightforward alternative would be "*there is a 95% probability that when computing a confidence interval from data of this sort, the effect falls within this range*". In general, the frequentist approach has been associated with the focus on null hypothesis testing, and the misuse of *p* values has been shown to critically contribute to the reproducibility crisis of psychological science (Chambers, Feredoes, Muthukumaraswamy, Suresh, & Etchells, 2014; Szucs & Ioannidis, 2016). There is a general agreement that the generalization of the Bayesian approach is a way of overcoming these issues (Benjamin et al., 2018; Etz & Vandekerckhove, 2016).

### What is the Bayesian Framework?

Once we agreed that the Bayesian framework is the right way to go, you might wonder what is the Bayesian framework. **What's all the fuss about?**

Omitting the maths behind it, let's just say that:

- The frequentist guy tries to estimate "the real effect". The "real" value of the correlation between X and Y. It returns a "point-estimate" (*i.e.*, a single value) of the "real" correlation (*e.g.*, r = 0.42), considering that the data is sampled at random from a "parent", usually normal distribution of data.
- **The Bayesian master assumes no such thing**. The data are what they are. Based on this observed data (and eventually from its expectations), the Bayesian sampling algorithm will return a probability distribution of the effect that is compatible with the observed data. For the correlation between X and Y, it will return a distribution that says "the most probable effect is 0.42, but this data is also compatible with correlations of 0.12 or 0.74".
- To characterize our effects, **no need of p values** or other mysterious indices. We simply describe the posterior distribution (*i.e.*, the distribution of the effect). We can present the median (better than the mean, as it actually means that the effect has 50% of chance of being higher and 50% of chance of being lower), the MAD (a median-based, robust equivalent of SD) and other stuff such as the 90% HDI, called here *credible interval*.


### The "Posterior" distribution



Let's imagine two numeric variables, Y and X. The correlation between them is r = -0.063 (p < .05). A Bayesian analysis would return the probability of distribution of this effect (the **posterior**), that we can characterize using several indices (centrality (**median** or mean), dispersion (SD or Median Absolute Deviation - **MAD**), etc.). Let's plot the posterior distribution of the possible correlation values that are compatible with our data.

<div class="figure" style="text-align: center">
<img src="bayesian_files/figure-html/unnamed-chunk-3-1.png" alt="Posterior probability distribution of the correlation between X and Y"  />
<p class="caption">Posterior probability distribution of the correlation between X and Y</p>
</div>

In this example (based on real data):

- The **median of the posterior distribution is really close to the *r* coefficient obtained through a "normal" correlation analysis**, appearing as a good point-estimate of the correlation. Moreover, and unlike the frequentist estimate, the median has an intuitive meaning (as it cuts the distribution in two equal parts): the "true" effect has 50% of chance of being higher and 50% of chance of being lower. 
- We can also compute the **90% *credible* interval**, which shows where the true effect can fall with a probability of 90% (better than 95% CI, see *Kruschke, 2015*). 
- Finally, we can also compute the **MPE**, *i.e.*, the maximum probability of effect, which is the probability that the effect is in the median's direction (*i.e.*, negative in this example): the area in red. It interesting to note that the probability that the effect is opposite ((100 - MPE) / 100, here (100-98.85) / 100 = 0.011) is relatively close to the actual **p value** obtained through frequentist correlation (p = 0.025).

**Now that you're familiar with posterior distributions, the core difference of the Bayesian framework, let's practice!**

## The affective Dataset

Let's start by taking a look at the dataset included within the `psycho` package.


```r
library(rstanarm)
library(dplyr)
library(ggplot2)
library(psycho)

df <- psycho::affective
summary(df)
```


```
##  Sex           Age        Birth_Season   Salary    Life_Satisfaction
##  F:1000   Min.   :18.00   Fall  :288   <1000:514   Min.   :1.000    
##  M: 251   1st Qu.:21.16   Spring:348   <2000:223   1st Qu.:4.000    
##           Median :22.97   Summer:332   2000+:128   Median :5.000    
##           Mean   :26.91   Winter:283   NA's :386   Mean   :4.847    
##           3rd Qu.:27.54                            3rd Qu.:6.000    
##           Max.   :80.14                            Max.   :7.000    
##    Concealing      Adjusting       Tolerating   
##  Min.   :0.000   Min.   :0.000   Min.   :0.500  
##  1st Qu.:2.750   1st Qu.:2.750   1st Qu.:3.500  
##  Median :3.750   Median :3.750   Median :4.250  
##  Mean   :3.743   Mean   :3.802   Mean   :4.157  
##  3rd Qu.:4.750   3rd Qu.:5.000   3rd Qu.:5.000  
##  Max.   :7.000   Max.   :6.750   Max.   :7.000
```

The data include **5 continuous variables** (age, life satisfaction and 3 affective styles) and **3 factors** (sex, salary and season of birth).



## Simple Regression (*Correlation*)

Let's start with something simple : a **correlation**. To simplify, a (Pearson's) correlation is pretty much nothing more than a simple linear regression (with standardized variables). Let's see if there's a linear relationship between **Life Satisfaction** and the tendency of **Tolerating** our emotions using a Bayesian linear regression model.

### Model Exploration


```r
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, data=df)
```




Let's check the results: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)
```

Variable       Median    MAD   CI_lower   CI_higher   MPE   Overlap
------------  -------  -----  ---------  ----------  ----  --------
(Intercept)      4.06   0.16       3.81        4.30    NA        NA
Tolerating       0.19   0.04       0.13        0.25   100      0.87
R2               0.02   0.01       0.01        0.04    NA        NA

For each parameter of the model, the summary shows:

- the **median** of the posterior distribution of the parameter (can be used as a point estimate, similar to the beta of frequentist models).
- the **Median Absolute Deviation (MAD)**, a robust measure of dispertion (could be seen as a robust version of SD).
- the **Credible Interval (CI)** (by default, the 90\% CI; see Kruschke, 2018), representing a range of possible parameter values.
- the **Maximum Probability of Effect (MPE)**, the probability that the effect is positive or negative (depending on the median’s direction).
- the **Overlap (O)**, the percentage of overlap between the posterior distribution and a normal distribution of mean 0 and same SD than the posterior. Can be interpreted as the probability that a value from the posterior distribution comes from a null distribution of same uncertainty (width).

It also returns the (unadjusted) R2 (which represents the *percentage of variance of the outcome explained by the model*). In the Bayesian framework, the R2 is also estimated with probabilities. As such, characteristics of its posterior distribution are returned.



We can also print a formatted version:

```r
print(results)
```

```
## We fitted a Markov Chain Monte Carlo gaussian (link = identity) model to predict Life_Satisfaction (formula = Life_Satisfaction ~ Tolerating). The model's priors were set as follows: 
## 
##   ~ normal (location = (0), scale = (3.11))
## 
## 
## The model explains about 2.33% of the outcome's variance (MAD = 0.0086, 90% CI [0.011, 0.038], Adj. R2 = 0.020). The intercept is at 4.06 (MAD = 0.16, 90% CI [3.81, 4.30]). Within this model:
## 
##   - The effect of Tolerating has a probability of 100% of being positive (Median = 0.19, MAD = 0.036, 90% CI [0.13, 0.25], O = 0.87%). It can be considered as small or very small with respective probabilities of 3.90% and 96.10%.
```

Note that the `print()` returns also additional info, such as the 100\% CI of the R2 and, for each parameter, the limits of the range defined by the MPE. 



### Interpretation


For now, omit the part dedicated to priors. We'll see it in the next chapters. Let's rather interpret the part related to effects.

> Full Bayesian mixed linear models are fitted using the rstanarm R wrapper for the stan probabilistic language (Gabry & Goodrich, 2016). Bayesian inference was done using Markov Chain Monte Carlo (MCMC) sampling. The prior distributions of all effects were set as weakly informative (mean = 0, SD = 3.11), meaning that we did not expect effects different from null in any particular direction. For each model and each coefficient, we will present several characteristics of the posterior distribution, such as its median (a robust estimate comparable to the beta from frequentist linear models), MAD (median absolute deviation, a robust equivalent of standard deviation) and the 90% credible interval. Instead of the *p value* as an index of effect existence, we also computed the maximum probability of effect (MPE), *i.e.*, the maximum probability that the effect is different from 0 in the median’s direction. For our analyses, we will consider an effect as inconsistent (*i.e.*, not probable enough) if its MPE is lower than 90% (however, **beware not to fall in a *p* value-like obsession**).


The current model explains about 2.33% of life satisfaction variance. Within this model, a positive linear relationship between life satisfaction and tolerating exists with high probability (Median = 0.19, MAD = 0.036, 90% CI [0.13, 0.25], MPE = 100%).

### Model Visualization

To visualize the model, the most neat way is to extract a "reference grid" (*i.e.*, a theorethical dataframe with balanced data).


```r
refgrid <- df %>% 
  select(Tolerating) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit, newdata=refgrid)
```

```r
predicted
```



 Tolerating   Life_Satisfaction_Median   Life_Satisfaction_CI_5   Life_Satisfaction_CI_95
-----------  -------------------------  -----------------------  ------------------------
   0.500000                   4.155194                 3.935241                  4.371941
   1.222222                   4.291746                 4.102592                  4.461981
   1.944444                   4.428112                 4.288622                  4.571845
   2.666667                   4.564777                 4.455533                  4.671763
   3.388889                   4.701402                 4.622224                  4.782610
   4.111111                   4.837788                 4.770952                  4.902808
   4.833333                   4.974813                 4.896355                  5.048800
   5.555556                   5.111209                 5.009771                  5.215158
   6.277778                   5.248139                 5.109542                  5.382569
   7.000000                   5.385332                 5.211227                  5.556997

Our refgrid is made of equally spaced (balanced) predictor values. It also include the median of the posterior prediction, as well as 90% credible intervals. Now, we can plot it as follows:


```r
ggplot(predicted, aes(x=Tolerating, y=Life_Satisfaction_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Life_Satisfaction_CI_5, 
                  ymax=Life_Satisfaction_CI_95), 
              alpha=0.1)
```

<img src="bayesian_files/figure-html/unnamed-chunk-13-1.png" style="display: block; margin: auto;" />


## Regression with Categorical Predictor (*ANOVA*)

When the predictor is categorical, simplifying the model is called running an ANOVA. Let's do it by answering the following question: does the level of **life satisfaction** depend on the salary? 

### Model Exploration


```r
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Salary, data=df)
```
Let's check the results: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
print(results)
```

```
## We fitted a Markov Chain Monte Carlo gaussian (link = identity) model to predict Life_Satisfaction (formula = Life_Satisfaction ~ Salary). The model's priors were set as follows: 
## 
##   ~ normal (location = (0, 0), scale = (3.61, 3.61))
## 
## 
## The model explains about 0.43% of the outcome's variance (MAD = 0.0039, 90% CI [0, 0.011], Adj. R2 = -0.0035). The intercept is at 4.77 (MAD = 0.065, 90% CI [4.66, 4.87]). Within this model:
## 
##   - The effect of Salary<2000 has a probability of 84.78% of being positive (Median = 0.13, MAD = 0.12, 90% CI [-0.072, 0.31], O = 60.29%). It can be considered as very small with a probability of 84.78%.
##   - The effect of Salary2000+ has a probability of 92.27% of being positive (Median = 0.20, MAD = 0.15, 90% CI [-0.041, 0.43], O = 49.06%). It can be considered as small or very small with respective probabilities of 0.025% and 92.25%.
```

### Post-hoc / Contrasts / Comparisons

What interest us is the pairwise comparison between the groups. The `get_contrasts` function computes the estimated marginal means (least-squares means), *i.e.*, the means of each group estimated by the model, as well as the contrasts.


```r
contrasts <- psycho::get_contrasts(fit, "Salary")
```

We can see the estimated means like that:

```r
contrasts$means
```

Level           Median    MAD   Mean     SD   CI_lower   CI_higher
-------------  -------  -----  -----  -----  ---------  ----------
Salary <1000      4.77   0.06   4.77   0.07       4.66        4.87
Salary <2000      4.89   0.10   4.89   0.10       4.73        5.05
Salary 2000+      4.97   0.13   4.97   0.13       4.74        5.17


And the contrasts comparisons like that:

```r
contrasts$contrasts
```

Contrast         Median    MAD    Mean     SD   CI_lower   CI_higher     MPE
--------------  -------  -----  ------  -----  ---------  ----------  ------
<1000 - <2000     -0.13   0.12   -0.12   0.12      -0.31        0.07   84.78
<1000 - 2000+     -0.20   0.15   -0.20   0.14      -0.43        0.04   92.27
<2000 - 2000+     -0.08   0.16   -0.08   0.16      -0.35        0.19   68.47

As we can see, the only probable difference (MPE > 90%) is between **Salary <1000** and **Salary 2000+**.

### Model Visualization


```r
ggplot(contrasts$means, aes(x=Level, y=Median, group=1)) +
  geom_line() +
  geom_pointrange(aes(ymin=CI_lower, ymax=CI_higher)) +
  ylab("Life Satisfaction") +
  xlab("Salary")
```

<img src="bayesian_files/figure-html/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />


## Logistic Regressions

Let's see if we can **predict the sex** with the tendency to flexibly *adjust* our emotional reactions. As the Sex is a binary factor (with two modalities), we have to fit a logistic model.

### Model Exploration


```r
# Let's fit our model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = "binomial")
```



First, let's check our model: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)
```

Variable       Median    MAD   CI_lower   CI_higher   MPE   Overlap
------------  -------  -----  ---------  ----------  ----  --------
(Intercept)     -2.19   0.22      -2.58       -1.87    NA        NA
Adjusting        0.21   0.05       0.13        0.29   100      4.07
R2               0.01   0.01       0.00        0.02    NA        NA

It appears that the link between adjusting and the sex is highly probable (MPE > 90%). But in what direction? To know that, we have to find out what is the intercept (the reference level).



```
## [1] "F" "M"
```
As **female** is the first level, it means that it is the intercept. Based on our model, an increase of 1 on the scale of **adjusting** will increase the probability (expressed in log odds ratios) of being a **male**.

### Model Visualization

To visualize this type of model, we have to derive a reference grid.


```r
refgrid <- df %>% 
  select(Adjusting) %>% 
  psycho::refdata(length.out=10)
  
predicted <- psycho::get_predicted(fit, newdata=refgrid)
```

Note that `get_predicted` automatically transformed log odds ratios (the values in which the model is expressed) to probabilities, easier to apprehend.


```r
ggplot(predicted, aes(x=Adjusting, y=Sex_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Sex_CI_5, 
                  ymax=Sex_CI_95), 
              alpha=0.1) +
  ylab("Probability of being a male")
```

<img src="bayesian_files/figure-html/unnamed-chunk-27-1.png" style="display: block; margin: auto;" />

We can nicely see the non-linear relationship between adjusting and the probability of being a male.

## Multiple Regressions and MANOVAs / ANCOVAs

Let's create models a bit more complex, mixing factors with numeric predictors, to see if the **life satisfaction** is related to the tendency to suppress, **conceal** the emotional reactions, and does this relationship depends on the **sex**.

### Model Exploration


```r
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Concealing * Sex, data=df)
```



Let's check our model: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)
```

Variable           Median    MAD   CI_lower   CI_higher     MPE   Overlap
----------------  -------  -----  ---------  ----------  ------  --------
(Intercept)          5.19   0.12       5.00        5.39      NA        NA
Concealing          -0.10   0.03      -0.15       -0.05   99.90     11.77
SexM                -0.67   0.32      -1.18       -0.15   98.10     30.24
Concealing:SexM      0.18   0.07       0.06        0.29   99.45     22.26
R2                   0.01   0.01       0.00        0.02      NA        NA

Again, it is important to notice that the intercept (the baseline) corresponds here to **Concealing = 0** and **Sex = F**. As we can see next, there is, with high probability, a negative linear relationship between concealing (*for females only*) and life satisfaction. Also, at the (theorethical) intercept (when concealing = 0), the males have a lower life satisfaction. Finally, the interaction is also probable. This means that when the participant is a male, the relationship between concealing and life satisfaction is significantly different (increased by 0.17. In other words, we could say that the relationship is of -0.10+0.17=0.07 in men).

### Model Visualization

How to represent this type of models? Again, we have to generate a reference grid.


```r
refgrid <- df %>% 
  select(Concealing, Sex) %>% 
  psycho::refdata(length.out=10)

predicted <- psycho::get_predicted(fit, newdata=refgrid)
predicted
```

 Concealing  Sex    Life_Satisfaction_Median   Life_Satisfaction_CI_5   Life_Satisfaction_CI_95
-----------  ----  -------------------------  -----------------------  ------------------------
  0.0000000  F                      5.190944                 4.995541                  5.387022
  0.7777778  F                      5.114785                 4.947997                  5.268818
  1.5555556  F                      5.038761                 4.916598                  5.166496
  2.3333333  F                      4.962730                 4.865183                  5.055351
  3.1111111  F                      4.886556                 4.812525                  4.963959
  3.8888889  F                      4.810286                 4.737004                  4.886059
  4.6666667  F                      4.735764                 4.646380                  4.827676
  5.4444444  F                      4.660024                 4.542577                  4.779455
  6.2222222  F                      4.583476                 4.432722                  4.735222
  7.0000000  F                      4.507004                 4.308682                  4.681965
  0.0000000  M                      4.522015                 4.049696                  5.026281
  0.7777778  M                      4.588182                 4.202320                  5.017525
  1.5555556  M                      4.653672                 4.321113                  4.979878
  2.3333333  M                      4.718625                 4.455494                  4.967686
  3.1111111  M                      4.782295                 4.584960                  4.965933
  3.8888889  M                      4.848196                 4.690425                  4.988641
  4.6666667  M                      4.911271                 4.764241                  5.062289
  5.4444444  M                      4.974947                 4.787171                  5.169754
  6.2222222  M                      5.038169                 4.778624                  5.284285
  7.0000000  M                      5.101102                 4.767042                  5.425294

As we can see, the reference grid is balanced in terms of factors and numeric predictors. Now, to plot this becomes very easy!



```r
ggplot(predicted, aes(x=Concealing, y=Life_Satisfaction_Median, fill=Sex)) +
  geom_line(aes(colour=Sex)) +
  geom_ribbon(aes(fill=Sex,
                  ymin=Life_Satisfaction_CI_5, 
                  ymax=Life_Satisfaction_CI_95), 
              alpha=0.1) +
  ylab("Life Satisfaction")
```

<img src="bayesian_files/figure-html/unnamed-chunk-33-1.png" style="display: block; margin: auto;" />

We can see that the error for the males is larger, due to less observations. 



## Mixed Models


### Why use mixed-models?

- **From Makowski et al. (*under review*):**

> The Mixed modelling framework allows estimated effects to vary by group at lower levels while estimating population-level effects through the specification of fixed (explanatory variables) and random (variance components) effects. Outperforming traditional procedures such as repeated measures ANOVA (Kristensen & Hansen, 2004), these models are particularly suited to cases in which experimental stimuli are heterogeneous (e.g., images) as the item-related variance, in addition to the variance induced by participants, can be accounted for (Baayen, Davidson, & Bates, 2008; Magezi, 2015). Moreover, mixed models can handle unbalanced data, nested designs, crossed random effects and missing data.

As for how to run this type of analyses, it is quite easy. Indeed, all what has been said previously remains the same for mixed models. Except that there are random effects (specified by putting `+ (1|random_term)` in the formula). For example, we might want to consider the **salary** as a random effect (to "**adjust**" (*so to speak*) for the fact that the data is structured in two groups). Let's explore the relationship between the tendency to **conceal** emotions and **age** (*adjusted* for **salary**).

### Model Exploration


```r
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ Age + (1|Salary), data=df)
```


Let's check our model: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)
```

Variable       Median    MAD   CI_lower   CI_higher    MPE   Overlap
------------  -------  -----  ---------  ----------  -----  --------
(Intercept)      3.95   0.17       3.61        4.25     NA        NA
Age             -0.01   0.00      -0.02        0.00   91.2     46.61
R2               0.00   0.00       0.00        0.01     NA        NA

As we can see, the linear relationship has only a moderate probability of being different from 0.

### Model Visualization



```r
refgrid <- df %>% 
  select(Age) %>% 
  psycho::refdata(length.out=10)

# We name the predicted dataframe by adding '_linear' to keep it for further comparison (see next part)
predicted_linear <- psycho::get_predicted(fit, newdata=refgrid)
```

```r
ggplot(predicted_linear, aes(x=Age, y=Concealing_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Concealing_CI_5, 
                  ymax=Concealing_CI_95), 
              alpha=0.1)
```

<img src="bayesian_files/figure-html/unnamed-chunk-39-1.png" style="display: block; margin: auto;" />

## Polynomial Transformations

Relationships in the real world are often non-linear. For example, based on the previous relationship between **concealing** and **age**, we could try modelling a polynomial (second order) transformation to the predictor.

### Model Exploration


```r
# Let's fit our model (it takes more time)
fit <- rstanarm::stan_lmer(Concealing ~ poly(Age, 2, raw=TRUE) + (1|Salary), data=df)
```


Let's check our model: 

```r
# Format the results using analyze()
results <- psycho::analyze(fit)

# We can extract a formatted summary table
summary(results, round = 2)
```

Variable                     Median    MAD   CI_lower   CI_higher    MPE   Overlap
--------------------------  -------  -----  ---------  ----------  -----  --------
(Intercept)                    5.04   0.52       3.98        5.84     NA        NA
poly(Age, 2, raw = TRUE)1     -0.07   0.03      -0.12       -0.01   99.2     26.15
poly(Age, 2, raw = TRUE)2      0.00   0.00       0.00        0.00   99.0     78.89
R2                             0.01   0.01       0.00        0.02     NA        NA

As we can see, both the linear relationship and the second order curvature are highly probable. However, when setting `raw=TRUE` in the formula, the coefficients become unintepretable. So let's visualize them.

### Model Visualization

The model visualization routine is similar to the previous ones.


```r
refgrid <- df %>% 
  select(Age) %>% 
  psycho::refdata(length.out=20)

predicted_poly <- psycho::get_predicted(fit, newdata=refgrid)
```

```r
ggplot(predicted_poly, aes(x=Age, y=Concealing_Median)) +
  geom_line() +
  geom_ribbon(aes(ymin=Concealing_CI_5, 
                  ymax=Concealing_CI_95), 
              alpha=0.1)
```

<img src="bayesian_files/figure-html/unnamed-chunk-45-1.png" style="display: block; margin: auto;" />

As we can see, adding the polynomial degree changes the relationship. Since the model is here very simple, we can add on the plot the actual points (however, they do not take into account the random effects and such), as well as plot the two models. Also, let's make it "dynamic" using `plotly`.


```r
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
```

<!--html_preserve--><div id="151ac435b5d19" style="width:672px;height:432px;" class="plotly html-widget"></div>
<script type="application/json" data-for="151ac435b5d19">{"x":{"data":[{"x":[18.002,24.9057777777778,31.8095555555556,38.7133333333333,45.6171111111111,52.5208888888889,59.4246666666667,66.3284444444444,73.2322222222222,80.136],"y":[3.81913580004789,3.76533604702256,3.70997199504797,3.65628501336272,3.60361499423766,3.5497829417289,3.50349882659925,3.45357056254459,3.40446738229186,3.35889462752493],"text":["Age: 18.00200<br />Concealing_Median: 3.819136","Age: 24.90578<br />Concealing_Median: 3.765336","Age: 31.80956<br />Concealing_Median: 3.709972","Age: 38.71333<br />Concealing_Median: 3.656285","Age: 45.61711<br />Concealing_Median: 3.603615","Age: 52.52089<br />Concealing_Median: 3.549783","Age: 59.42467<br />Concealing_Median: 3.503499","Age: 66.32844<br />Concealing_Median: 3.453571","Age: 73.23222<br />Concealing_Median: 3.404467","Age: 80.13600<br />Concealing_Median: 3.358895"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"rgba(0,0,255,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[18.002,24.9057777777778,31.8095555555556,38.7133333333333,45.6171111111111,52.5208888888889,59.4246666666667,66.3284444444444,73.2322222222222,80.136,80.136,80.136,73.2322222222222,66.3284444444444,59.4246666666667,52.5208888888889,45.6171111111111,38.7133333333333,31.8095555555556,24.9057777777778,18.002,18.002],"y":[3.62769772451571,3.57381970121673,3.55854506379832,3.48056044344003,3.38146248408341,3.31134249461444,3.1859557986728,3.06056910273115,2.91637331471488,2.87562978282457,2.87562978282457,3.83161390279063,3.76224890598338,3.79583520398144,3.81902962570911,3.84125985883342,3.8182755860335,3.84765920919619,3.90322832240856,3.91638859004934,4.01904619997944,3.62769772451571],"text":["Age: 18.00200<br />Concealing_CI_5: 3.627698<br />Concealing_CI_95: 4.019046","Age: 24.90578<br />Concealing_CI_5: 3.573820<br />Concealing_CI_95: 3.916389","Age: 31.80956<br />Concealing_CI_5: 3.558545<br />Concealing_CI_95: 3.903228","Age: 38.71333<br />Concealing_CI_5: 3.480560<br />Concealing_CI_95: 3.847659","Age: 45.61711<br />Concealing_CI_5: 3.381462<br />Concealing_CI_95: 3.818276","Age: 52.52089<br />Concealing_CI_5: 3.311342<br />Concealing_CI_95: 3.841260","Age: 59.42467<br />Concealing_CI_5: 3.185956<br />Concealing_CI_95: 3.819030","Age: 66.32844<br />Concealing_CI_5: 3.060569<br />Concealing_CI_95: 3.795835","Age: 73.23222<br />Concealing_CI_5: 2.916373<br />Concealing_CI_95: 3.762249","Age: 80.13600<br />Concealing_CI_5: 2.875630<br />Concealing_CI_95: 3.831614","Age: 80.13600<br />Concealing_CI_5: 2.875630<br />Concealing_CI_95: 3.831614","Age: 80.13600<br />Concealing_CI_5: 2.875630<br />Concealing_CI_95: 3.831614","Age: 73.23222<br />Concealing_CI_5: 2.916373<br />Concealing_CI_95: 3.762249","Age: 66.32844<br />Concealing_CI_5: 3.060569<br />Concealing_CI_95: 3.795835","Age: 59.42467<br />Concealing_CI_5: 3.185956<br />Concealing_CI_95: 3.819030","Age: 52.52089<br />Concealing_CI_5: 3.311342<br />Concealing_CI_95: 3.841260","Age: 45.61711<br />Concealing_CI_5: 3.381462<br />Concealing_CI_95: 3.818276","Age: 38.71333<br />Concealing_CI_5: 3.480560<br />Concealing_CI_95: 3.847659","Age: 31.80956<br />Concealing_CI_5: 3.558545<br />Concealing_CI_95: 3.903228","Age: 24.90578<br />Concealing_CI_5: 3.573820<br />Concealing_CI_95: 3.916389","Age: 18.00200<br />Concealing_CI_5: 3.627698<br />Concealing_CI_95: 4.019046","Age: 18.00200<br />Concealing_CI_5: 3.627698<br />Concealing_CI_95: 4.019046"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(0,0,255,0.1)","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[18.002,21.2722105263158,24.5424210526316,27.8126315789474,31.0828421052632,34.3530526315789,37.6232631578947,40.8934736842105,44.1636842105263,47.4338947368421,50.7041052631579,53.9743157894737,57.2445263157895,60.5147368421053,63.7849473684211,67.0551578947368,70.3253684210526,73.5955789473684,76.8657894736842,80.136],"y":[3.99053649686244,3.85560811069481,3.74132360578134,3.64880329192715,3.56964105630442,3.50627499675606,3.46121238542074,3.43085310776214,3.4240179147981,3.43401191753256,3.46022106824437,3.5067137133603,3.57544733913644,3.65360233606577,3.74695001120739,3.86006444494821,3.99574355845738,4.14881480883104,4.31772228022067,4.52652690943617],"text":["Age: 18.00200<br />Concealing_Median: 3.990536","Age: 21.27221<br />Concealing_Median: 3.855608","Age: 24.54242<br />Concealing_Median: 3.741324","Age: 27.81263<br />Concealing_Median: 3.648803","Age: 31.08284<br />Concealing_Median: 3.569641","Age: 34.35305<br />Concealing_Median: 3.506275","Age: 37.62326<br />Concealing_Median: 3.461212","Age: 40.89347<br />Concealing_Median: 3.430853","Age: 44.16368<br />Concealing_Median: 3.424018","Age: 47.43389<br />Concealing_Median: 3.434012","Age: 50.70411<br />Concealing_Median: 3.460221","Age: 53.97432<br />Concealing_Median: 3.506714","Age: 57.24453<br />Concealing_Median: 3.575447","Age: 60.51474<br />Concealing_Median: 3.653602","Age: 63.78495<br />Concealing_Median: 3.746950","Age: 67.05516<br />Concealing_Median: 3.860064","Age: 70.32537<br />Concealing_Median: 3.995744","Age: 73.59558<br />Concealing_Median: 4.148815","Age: 76.86579<br />Concealing_Median: 4.317722","Age: 80.13600<br />Concealing_Median: 4.526527"],"type":"scatter","mode":"lines","line":{"width":3.77952755905512,"color":"rgba(255,0,0,1)","dash":"solid"},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[18.002,21.2722105263158,24.5424210526316,27.8126315789474,31.0828421052632,34.3530526315789,37.6232631578947,40.8934736842105,44.1636842105263,47.4338947368421,50.7041052631579,53.9743157894737,57.2445263157895,60.5147368421053,63.7849473684211,67.0551578947368,70.3253684210526,73.5955789473684,76.8657894736842,80.136,80.136,80.136,76.8657894736842,73.5955789473684,70.3253684210526,67.0551578947368,63.7849473684211,60.5147368421053,57.2445263157895,53.9743157894737,50.7041052631579,47.4338947368421,44.1636842105263,40.8934736842105,37.6232631578947,34.3530526315789,31.0828421052632,27.8126315789474,24.5424210526316,21.2722105263158,18.002,18.002],"y":[3.64232607171662,3.5896827675883,3.51515423825869,3.43346204796438,3.32668698798604,3.25682195635209,3.18517658634401,3.15522162998699,3.12848418549071,3.14857882497369,3.2067733219518,3.20962507644616,3.20905842486615,3.26866329588963,3.27521076729579,3.33074738029496,3.33816448434374,3.32876653697512,3.52511630745697,3.53105428920721,3.53105428920721,5.53513058322363,5.21942532144045,4.75109001784307,4.531865313865,4.33171760992847,4.09407429853441,3.95873005073373,3.82394099845164,3.76390756371898,3.75718717032721,3.68902226909539,3.68348779036466,3.70025389039527,3.7036730543079,3.74539483186088,3.80231852207344,3.90171821081169,3.98918748231297,4.1086855053226,4.2342206182356,3.64232607171662],"text":["Age: 18.00200<br />Concealing_CI_5: 3.642326<br />Concealing_CI_95: 4.234221","Age: 21.27221<br />Concealing_CI_5: 3.589683<br />Concealing_CI_95: 4.108686","Age: 24.54242<br />Concealing_CI_5: 3.515154<br />Concealing_CI_95: 3.989187","Age: 27.81263<br />Concealing_CI_5: 3.433462<br />Concealing_CI_95: 3.901718","Age: 31.08284<br />Concealing_CI_5: 3.326687<br />Concealing_CI_95: 3.802319","Age: 34.35305<br />Concealing_CI_5: 3.256822<br />Concealing_CI_95: 3.745395","Age: 37.62326<br />Concealing_CI_5: 3.185177<br />Concealing_CI_95: 3.703673","Age: 40.89347<br />Concealing_CI_5: 3.155222<br />Concealing_CI_95: 3.700254","Age: 44.16368<br />Concealing_CI_5: 3.128484<br />Concealing_CI_95: 3.683488","Age: 47.43389<br />Concealing_CI_5: 3.148579<br />Concealing_CI_95: 3.689022","Age: 50.70411<br />Concealing_CI_5: 3.206773<br />Concealing_CI_95: 3.757187","Age: 53.97432<br />Concealing_CI_5: 3.209625<br />Concealing_CI_95: 3.763908","Age: 57.24453<br />Concealing_CI_5: 3.209058<br />Concealing_CI_95: 3.823941","Age: 60.51474<br />Concealing_CI_5: 3.268663<br />Concealing_CI_95: 3.958730","Age: 63.78495<br />Concealing_CI_5: 3.275211<br />Concealing_CI_95: 4.094074","Age: 67.05516<br />Concealing_CI_5: 3.330747<br />Concealing_CI_95: 4.331718","Age: 70.32537<br />Concealing_CI_5: 3.338164<br />Concealing_CI_95: 4.531865","Age: 73.59558<br />Concealing_CI_5: 3.328767<br />Concealing_CI_95: 4.751090","Age: 76.86579<br />Concealing_CI_5: 3.525116<br />Concealing_CI_95: 5.219425","Age: 80.13600<br />Concealing_CI_5: 3.531054<br />Concealing_CI_95: 5.535131","Age: 80.13600<br />Concealing_CI_5: 3.531054<br />Concealing_CI_95: 5.535131","Age: 80.13600<br />Concealing_CI_5: 3.531054<br />Concealing_CI_95: 5.535131","Age: 76.86579<br />Concealing_CI_5: 3.525116<br />Concealing_CI_95: 5.219425","Age: 73.59558<br />Concealing_CI_5: 3.328767<br />Concealing_CI_95: 4.751090","Age: 70.32537<br />Concealing_CI_5: 3.338164<br />Concealing_CI_95: 4.531865","Age: 67.05516<br />Concealing_CI_5: 3.330747<br />Concealing_CI_95: 4.331718","Age: 63.78495<br />Concealing_CI_5: 3.275211<br />Concealing_CI_95: 4.094074","Age: 60.51474<br />Concealing_CI_5: 3.268663<br />Concealing_CI_95: 3.958730","Age: 57.24453<br />Concealing_CI_5: 3.209058<br />Concealing_CI_95: 3.823941","Age: 53.97432<br />Concealing_CI_5: 3.209625<br />Concealing_CI_95: 3.763908","Age: 50.70411<br />Concealing_CI_5: 3.206773<br />Concealing_CI_95: 3.757187","Age: 47.43389<br />Concealing_CI_5: 3.148579<br />Concealing_CI_95: 3.689022","Age: 44.16368<br />Concealing_CI_5: 3.128484<br />Concealing_CI_95: 3.683488","Age: 40.89347<br />Concealing_CI_5: 3.155222<br />Concealing_CI_95: 3.700254","Age: 37.62326<br />Concealing_CI_5: 3.185177<br />Concealing_CI_95: 3.703673","Age: 34.35305<br />Concealing_CI_5: 3.256822<br />Concealing_CI_95: 3.745395","Age: 31.08284<br />Concealing_CI_5: 3.326687<br />Concealing_CI_95: 3.802319","Age: 27.81263<br />Concealing_CI_5: 3.433462<br />Concealing_CI_95: 3.901718","Age: 24.54242<br />Concealing_CI_5: 3.515154<br />Concealing_CI_95: 3.989187","Age: 21.27221<br />Concealing_CI_5: 3.589683<br />Concealing_CI_95: 4.108686","Age: 18.00200<br />Concealing_CI_5: 3.642326<br />Concealing_CI_95: 4.234221","Age: 18.00200<br />Concealing_CI_5: 3.642326<br />Concealing_CI_95: 4.234221"],"type":"scatter","mode":"lines","line":{"width":1.88976377952756,"color":"transparent","dash":"solid"},"fill":"toself","fillcolor":"rgba(255,0,0,0.1)","hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null},{"x":[25.038,25.873,22.199,27.259,49.022,48.811,20.214,22.58,30.164,23.179,24.496,26.273,19.286,25.014,52.508,23.179,25.129,31.587,24.072,20.855,21.884,24.395,22.758,21.462,57.239,22.418,23.729,21.109,21.942,23.029,21.337,22.117,22.763,23.387,24.595,22.33,22.298,23.264,25.03,20.069,21.356,38.689,22.623,22.355,19.072,23.948,21.753,21.605,21.246,23.149,22.53,18.944,21.66,52.636,40.127,22.33,23.65,20.942,22.467,21.983,23.108,22.678,64.215,21.065,23.729,27.667,18.897,21.408,21.101,29.255,21.41,20.964,21.172,24.909,21.646,20.822,26.796,22.659,24.493,18.276,23.127,19.683,48.445,24.37,20.688,20.532,25.014,21.386,24.006,22.777,22.689,23.792,21.624,28.061,26.963,21.087,37.86,20.373,21.156,23.001,21.194,22.53,22.473,31.459,21.536,67.144,19.817,24.704,26.456,21.922,22.506,22.626,26.758,22.141,23.094,21.227,23.779,26.566,23.119,19.223,25.449,23.647,22.966,24.671,22.215,21.849,58.274,25.287,21.315,35.579,24.241,22.96,22.81,24.458,19.294,21.832,18.9,20.011,19.664,21.583,19.65,22.339,21.312,21.734,30.615,22.314,24.589,23.458,22.867,19.376,22.618,21.613,23.324,25.331,19.658,22.643,19.957,45.822,23.428,23.261,19.319,20.496,19.757,21.175,19.519,23.888,22.284,21.769,22.478,24.48,40.236,21.23,21.405,21.714,21.085,21.933,20.46,19.587,18.788,21.525,32.54,22.437,24.805,21.306,21.435,22.308,40.915,20.406,21.632,20.855,23.508,24.48,21.734,19.959,30.996,18.221,22.15,24.8,22.013,20.866,25.369,20.351,22.883,23.05,20.148,22.393,30.519,21.605,23.612,21.43,29.109,23.294,21.482,21.958,20.972,21.649,21.972,20.458,24.014,21.211,23.549,22.525,21.788,19.56,25.435,23.647,22.24,18.952,23.458,21.243,24.239,21.274,22.59,37.08,21.454,19.686,46.895,22.185,19.891,22.139,19.239,21.46,21.361,52.026,25.621,36.97,24.496,19.806,42.895,21.29,21.479,21.087,21.884,20.299,20.912,21.859,22.552,23.431,21.216,22.415,20.055,24.228,22.872,23.081,21.701,26.295,26.391,21.654,23.617,25.61,20.69,23.817,25.085,21.109,29.244,32.891,21.602,22.174,23.656,22.67,23.193,21.558,44.245,27.951,27.212,26.875,30.555,21.92,31.292,28.436,23.13,40.206,33.83,23.105,33.055,20.773,21.232,27.22,19.313,19.669,42.323,29.205,19.321,25.512,22.024,21.367,19.19,28.033,22.632,24.838,20.066,19.004,18.377,22.057,24.822,22.736,74.561,23.497,25.153,22.711,25.78,57.855,45.165,27.924,20.863,21.331,23.784,23.557,20.504,20.438,31.582,24.696,23.297,18.935,21.345,25.145,20.334,21.542,24.085,18.495,70.318,21.712,19.831,23.995,23.097,21.898,25.449,20.23,22.563,18.536,21.917,19.434,22.59,23.656,22.585,19.743,19.768,22.774,22.508,21.432,22.278,28.061,23.187,24.762,21.265,20.556,29.378,48.683,19.19,18.971,20.455,30.64,20.647,20.135,22.361,21.561,19.948,19.094,30.268,25.421,19.469,22.306,23.719,30.216,22.226,25.958,19.22,19.584,18.859,32.48,22.826,21.134,22.653,33.534,26.09,21.966,23.25,19.842,27.543,20.269,28.482,19.267,23.825,18.922,20.723,21.736,20.575,22.582,18.355,31.483,20.214,20.534,25.224,21.849,18.558,23.409,21.408,21.339,28.827,21.238,21.879,23.656,31.409,19.371,22.109,25.498,22.026,23.379,18.021,19.467,25.52,21.29,25.766,20.603,39.749,19.976,22.64,27.094,23.144,33.504,20.334,24.345,24.017,61.436,19.256,23.245,70,20.466,25.931,20.274,21.301,19.36,22.1,20.419,21.914,50.46,20.904,20.699,20.953,20.436,40.203,19.579,21.646,26.916,19.354,18.47,22.662,20.54,21.137,30.415,26.7,22.292,20.211,20.909,21.405,19.037,58.583,19.428,22.692,22.771,23.215,18.078,18.07,28.244,24.214,18.454,19.924,21.416,21.512,20.543,18.987,20.063,19.888,21.372,19.379,19.102,29.112,22.314,58.526,19.798,51.823,21.068,19.174,24.37,19.62,18.095,25.235,29.008,24.17,18.401,22.35,46.112,18.002,20.285,21.906,22.569,23.839,18.136,19.801,20.975,19.267,20.901,22.656,21.599,22.089,38.106,27.19,22.517,30.402,24.332,22.232,19.836,18.445,21.525,24.329,19.447,18.897,19.434,35.095,22.21,21.309,33.249,20.039,19.874,19.379,21.87,18.308,18.733,33.17,19.231,49.011,27.532,47.432,21.317,22.555,44.494,22.051,19.623,18.147,19.245,19.447,19.185,19.71,22.081,25.517,20.014,28.485,20.003,22.188,20.625,18.812,21.219,66.862,28.001,32.242,18.722,22.429,20.241,21.556,20.036,21.755,19.412,62.602,23.059,23.478,20.762,53.247,23.275,18.256,18.511,25.838,19.053,56.595,25.881,40.264,20.324,19.601,19.289,18.886,25.627,22.084,22.084,20.261,19.428,19.261,50.955,20.477,28.819,60.645,21.547,19.456,80.136,31.847,36.655,41.802,18.708,18.922,22.352,29.036,52.877,25.306,19.165,29.884,19.371,19.683,23.152,20.348,19.727,29.003,24.34,25.463,24.162,20.986,21.482,24.71,21.257,40.247,27.431,50.602,25.066,24.784,21.964,20.43,21.635,23.291,25.334,22.38,26.779,19.425,25.734,23.374,19.179,34.881,23.886,24.425,25.851,21.813,28.324,18.924,22.612,36.499,19.25,23.628,26.886,34.259,20.055,20.934,58.011,21.684,19.258,19.642,25.085,21.777,23.346,54.931,21.879,20.033,40.475,22.782,19.456,21.246,21.279,29.725,31.308,42.531,21.706,26.878,22.35,20.748,20.931,21.873,66.077,18.985,70.342,28.702,19.343,18.503,19.1,30.725,20.367,55.61,19.757,20.365,60.155,55.139,37.153,48.045,39.464,21.717,42.227,27.097,20.055,23.875,27.973,23.82,60.439,19.483,20.189,32.039,20.975,20.058,19.68,22.886,52.48,21.309,26.131,19.576,21.126,19.724,23.639,18.139,20.868,19.546,22.84,32.499,23.1,20.23,20.997,26.684,23.297,25.731,44.31,23.59,36.814,48.806,18.807,60.65,66.764,24.272,21.449,22.122,49.967,32.748,55.93,21.096,27.806,20.009,45.436,20.655,47.971,21.567,35.795,57.384,26.864,19.195,19.634,24.203,44.71,21.419,21.95,19.965,21.616,20.269,21.178,22.793,23.699,21.597,24.146,23.538,22.377,49.775,23.921,22.7,29.594,21.139,22.361,50.942,20.789,22.306,22.229,20.559,19.792,21.884,23.28,22.292,22.886,20.699,24.091,23.636,50.468,33.55,18.473,20.518,23.228,26.111,42.67,23.751,29.892,23.765,20.668,21.276,36.154,18.894,23.97,26.637,25.933,23.683,44.324,20.033,25.287,50.942,20.638,20.874,36.502,22.952,30.478,20.764,28.978,62.148,30.358,23.858,21.194,21.32,31.229,31.732,22.081,22.878,19.694,38.832,25.178,19.19,19.07,22.758,21.52,38.558,21.386,29.003,29.493,23.992,20.496,21.687,22.925,28.006,23.426,19.907,19.948,21.758,23.223,18.363,18.579,22.092,21.386,49.512,22.005,20.096,25.093,55.415,28.863,21.703,21.939,20.679,26.738,22.818,23.034,24.121,32.458,28.83,33.156,22.152,21.293,52.82,22.604,48.609,18.815,29.676,28.762,29.345,24.51,25.123,23.604,23.404,24.567,44.354,27.918,24.077,22.653,33.909,49.874,23.82,31.645,23.513,20.271,22.369,26.936,27.825,53.992,26.027,19.089,24.707,30.396,20.422,18.829,22.883,20.094,28.97,21.972,20.219,27.434,22.325,23.51,24.022,24.789,18.84,25.134,23.497,26.591,31.278,26.508,24.877,21.003,25.84,32.028,32.559,34.051,21.484,21.29,24.181,29.6,21.783,32.567,20.559,18.254,20.094,31.762,25.345,39.464,20.907,20.022,19.253,18.944,23.182,31.543,25.254,23.217,19.937,25.38,41.731,28.012,30.615,46.947,20.425,22.281,22.64,21.397,25.63,35.91,66.939,20.972,25.846,20.395,44.483,29.224,23.212,50.72,55.563,57.381,23.99,21.944,19.595,31.749,21.87,24.225,36.776,23.587,20.044,25.756,29.98,33.112,30.519,21.545,22.933,29.192,22.878,23.237,22.407,20.367,25.378,24.937,21.999,24.43,27.067,39.7,28.94,21.517,71.199,20.2,42.635,28.332,23.724,50.818,33.167,26.027,55.897,25.747,18.971,56.081,26.667,24.482,23.133,21.484,21.484,20.252,22.733,58.605,18.694,20.512,22.289,31.757,19.124,40.532,42.903,32.776,26.536,19.124,24.526,22.998,24.559,26.842,22.339,29.46,19.42,25.98,59.782,24.332,48.716,22.018,25.813,26.155,20.318,58.148,29.323,30.645,27.576,28.66,23.699,21.191,32.384,26.429,18.894,24.239,54.509,62.443,20.449,23.891,27.568,28.513,22.845,49.731,22.517,37.487,19.406,35.382,24.909,25.186,20.553,21.416,33.126,38.249,70.063,61.51,19.149,35.27,30.279,26.396,61.214,22.618,20.77,23.729,58.416,24.909,27.483,25.334,27.798,33.033,22.336,20.95,45.657,67.692,24.548,22.533,51.716,43.451,51.196,41.101,22.27,33.392,22.53,21.019,30.725,23.921,36.39,43.856,22.224,56.083,40.193,67.385,62.509,38.287,20.376,37.83,44.631,68.459,60.242,20.324,37.301,21.367,26.24,24.918,24.189,59.615,26.569,24.699,22.07,28.162,19.989,31.519,54.493,66.63,58.608,61.228,21.205,23.267,34.544,25.509,59.662,20.047,29.408,21.408,50.372,27.702,24.151,34.933,22.974,21.742,34.221,23.817,20.589,36.006,23.376,22.032,24.398,22.653,22.549,24.847,21.082,22.612,23.743,20.551,23.261,22.697,21.186,34.837,22.388,18.265,55.621,22.555,22.648,23.067,23.932,21.744,22.136,28.096,22.021,62.348,33.695,32.458,22.473,25.531,30.55,24.126,23.554,27.609,27.346,35.962,33.802,23.708,62.893,33.684,25.558,23.327,22.114,23.365,22.475,32.904,23.891,21.961,23.562,25.104,22.418,24.236,21.706,24.912,23.308,19.795,24.562,42.67,23.168,22.656,20.2,18.549,29.819,24.135,24.808,22.519,23.22,23.951,30.331,41.794,22.768,22.705,33.537,18.281,21.783,22.785,24.422,22.601,37.542,22.467,23.694,19.833,33.082,21.936,22.612,41.772,45.531,23.478,23.666,25.044,25.386,27.242,42.35],"y":[0.75,4.5,1.5,3.75,2,1.75,4.25,6.75,6,1,2.75,5,2.75,3.25,0.5,5.75,5.25,2,2.25,6.5,3.5,4.25,4,3,1.5,4,3.5,4.5,3.75,2,2,5,6.25,4.25,3.75,6,0.75,5.25,4.5,3.25,4.75,4.5,2.75,2.25,5.5,2.75,4.75,3,6.25,2.75,1.5,3.5,1.5,3.25,4.25,0.75,6.5,3.5,4.5,7,2,4,5.5,4,4.25,1.25,4,3.5,5,0.25,4.75,1.5,5,5.25,6.25,3.5,2.5,2.5,1.25,3.5,6.25,3.25,4.75,4.75,1.25,3.25,4.25,6.25,1.5,5.25,4,5.75,5.25,1.25,2.25,3,4,4,3.5,6.75,0.75,5.75,2.75,4.25,5,3.25,2.75,5,3.75,3,4.75,2.5,3.5,2.75,1.75,2.75,3.25,4,3.5,3.5,1.25,0,4.25,3,5.5,3.5,4.75,2.75,2,4,3.25,5,3.5,1.75,2.75,4.25,4.5,1.75,4.25,3.5,2.75,5.75,2,2.25,6.5,4.5,1.5,2.5,6,0.5,5.25,3,3.25,5.25,2.25,2.5,5.25,5,4,4.5,6,4,4.25,1.75,1.25,2.75,4.5,3,3.25,4.75,1.75,3.75,4.5,3.5,3.75,6.75,1.5,6,4.25,6,3.5,6.25,4.5,2.75,4.25,5.25,3.75,3.5,4,3.75,4.5,5.25,3.5,5.75,2,3,1.25,5,3.25,2.75,4.5,6.5,1.75,4.5,5.5,5.25,6.25,4,5.25,5.25,6,4.5,1,4.5,3.5,3.5,6,3.75,3,4.75,4.5,5.75,3.75,1,0.75,4.5,3.75,3.25,1,4.25,5.5,4,7,5.25,3.75,4.25,3.25,2.25,2.5,4,2.75,2.5,3,3,2.5,5,3.75,4.25,1.75,5,2.75,4.75,1.75,0.25,4.25,3.75,6.25,3.25,2.5,5,3,4,3,4.75,2.5,1.5,5,4,3.75,3.5,5.75,3,2.75,4.25,2,5,3.75,3.5,2.25,4,6.75,5.25,1,2,2.5,3.5,4.75,1.5,4.75,4,2.75,2,3.75,3.5,4.25,4,2.5,3.25,4.25,5.5,3.5,4.75,3.5,4.25,5.5,3.25,4.25,4.25,5.75,3.75,2.5,4.75,5,1.75,2.25,3,4.25,4.25,0.75,6,7,4.25,3.25,4.25,2.25,2.75,4,3,6.25,5.5,3.75,3.75,3,3.25,3.5,3.5,5.75,4.75,4.75,4.25,5,3.75,4.75,4.5,3.75,4.75,3.25,5.5,2,3.5,3.5,3.75,3.5,5.25,5.5,3.25,4,2.25,3,4.25,2.75,4,3,3.75,4.5,1.75,5.75,4.25,3.25,3.5,1.75,6.75,4.75,4.25,3.75,6.5,1.25,4.75,2.75,6.75,2.75,3,4,2.5,4.25,2.75,2,2.25,2,4.75,5,5.25,4.5,5.25,2,4.5,4.25,5.25,5,2.75,1.25,2,4,1.25,5.75,4,1,1.25,2.5,2.5,4,2,2.75,1.25,3.75,6.25,5.25,6,3,4,5.25,2,4,3.75,4.5,3.5,2.75,5.25,7,5,3,6.25,6.75,4.75,2.5,5,3.25,3.25,5.5,2.5,5,1.5,3,2,5,4.75,5.25,2.75,1.5,4.25,4,4.5,3.25,4.25,3,5.75,5,3.5,2.5,1.5,1.75,5.5,4,3.75,5.25,3.25,2.75,2,4.25,4,1.75,4.25,2.75,3,4,3.25,2.25,4.75,4,2.25,1.25,4.75,6.5,4.25,4,4.5,4.25,2.5,3,2.5,3,4,4,1.75,4.25,4,2,5.5,3.25,3.25,4.5,6,3.75,2.75,1.5,4.25,3,0.75,5,5.75,4.25,3.5,3.75,1,5,2,5.75,1.75,1,4.75,4.75,5.5,3.75,7,5,3.75,3.5,6,2.5,5.75,2.5,3.75,4,3.25,4,4.25,6.25,4.25,5.5,3.25,4,6.75,2.75,2.25,4,2.5,3.25,4,6.25,3.25,3.75,5.75,3.75,5,4.5,3.5,5,6,4,4.5,6,3.5,6.75,4.25,2.75,3.5,2,2.25,6.25,2.5,2.5,0.5,5.75,4.75,2,2.5,3.5,7,5.25,6,4.25,4,4.25,1.75,4.5,5.5,2.75,2.25,0.75,4.25,6.25,2,1.25,4.25,5.25,3.5,4.75,2.5,3.75,4,3,4,3,2.75,6.25,2.75,4,3.5,3.5,5,4.5,6.25,4.25,2.25,1.75,5,3.25,1,1,4.5,5.5,6.5,4,2.5,2.5,5.75,4.75,3.75,3.25,6.25,4.25,1.75,2,3,5.25,4.5,3.25,4.25,4,4.25,4.5,6.5,7,4.25,5,1.25,2.25,1.75,0.75,2,5,3.75,0.75,4.25,4,4.75,3.75,3.25,2.5,7,3.25,3,4,5.5,3.25,6,5.25,4.75,4.75,4.75,1.75,4.5,3.75,7,4,4.75,3.75,3.25,6.75,6.75,5.25,3.5,5.25,2.75,3.5,3,6.25,3.5,4.75,2.75,2.25,1.25,3.25,4.75,3.5,2,4.25,3.25,4.75,6.75,4,4,3,2.5,5.5,4.25,3.25,3.75,7,5,2.75,7,5.75,2,2.25,5.75,4.75,1,1.25,3,3.75,4,4,2.25,3,3.25,3.5,1.75,3.75,2,3,6.25,6,3.25,0.25,4.25,4.25,4,3,2.75,0,4.75,3,2.25,2.25,4.75,4.75,5.75,6,6.5,2.25,4.75,2.5,2.25,3.25,4.75,2,2,2.25,6.25,6,3,0.75,4.75,2.5,6,3,1.5,3,5,6,4.5,2.25,4.5,5.5,1.5,4.75,3.25,5.5,3.5,5,3,5.5,4.5,2.5,2,2,2.5,4.25,5.25,4.5,5.75,5.75,2.75,5.75,2.5,2.25,6,2.5,0.75,3.75,5.75,4,2,1.75,4.75,4,1.75,4.5,1.25,4,3.5,3.5,5.25,2,1.25,5,3.5,3.25,5.25,2,5,3.25,1.25,3.5,2.75,2.5,6.5,2.25,1.5,3.75,3,5.5,5,1.75,4.5,4,1.5,3.75,5.25,3.75,4.5,4.5,3.5,5.5,3.75,1.25,6,4.75,3.5,0.75,1.5,4.5,4.25,0.75,4.75,3.5,2.75,6.25,1.25,5,4.25,4.5,1,6.75,5,5.25,2.5,5,2.5,5,5.75,3,3.25,4.75,6.25,4.25,5.25,4.25,4.5,2.5,3.5,1.75,5.5,5.25,0,6.5,4.75,6.25,2.75,4.5,5,3,2.5,2.75,6.25,3.75,4.25,3.75,3.5,4.25,4.5,5.75,1.75,4,2.75,1.75,1.75,2.75,4.5,3,2.5,2.5,3,3.25,2,4,5.5,3.25,4.5,6.5,4,5.75,4.5,5.5,6.5,2.5,3.5,3.25,2,2.5,3.25,4.25,2,4.75,5.25,4.75,5.25,4.5,0.75,4.25,3,4.25,5.25,2.75,4,6,4.75,4.75,2.5,1.25,2.5,2.75,3.25,6.5,5.25,4.5,5.5,4,5.75,3.75,5,4.5,5,2.5,4.5,3.25,3.25,6.75,6,3.25,4,3.75,3.25,3.75,3.75,2.25,2,2,4.75,3.5,5,6.5,1.75,6,0.75,3,4,3.25,3.5,2.25,4,5.25,2.5,5.5,4.5,1.75,4.75,1.75,2.75,1.75,3.25,4,4,5.25,3.75,0.5,2.5,5.5,6.25,4,5,1.75,5.25,3.25,2.25,0.75,5.5,4.75,4.5,4.5,0.5,6.75,3.25,6.25,5,2.75,3.25,4,5.75,3.75,3.75,5.25,4,2.75,5.5,6.25,2.5,4,6.5,2.5,2.5,2,3.5,4.25,4.5,6.75,1.25,2.5,2.5,1.75,3.5,2.25,4.75,5.25,4.25,2.5,4.5,5.25,4.25,3.25,5,5.5,4.25,2.5,6,4,4.5,3.5,5.25,4.25,3,3,3.75,4,6,4,2.5,5,7,2.5,4.75,4.25,2.5,5.5,3.5,0,5.75,3,2.75,6.75,5.75,4.75,4.75,3.75,5,4.25,2.25,0.25,4.5,4.25,4.5,5.25,6.25,4.5,0.5,5,5,7,5.25,1.25,6.25,2.25,3.25,3,1.75,4.5,2.75,4.5,2.25,3,2.25,4,4,3.5,4.25,3.75,4,3.5,4.25,3.75,3.25,3.75,4.25,1.25,3.25,5,2.75,1,4,4,2,3.5,4.25,1,1.75,2.5,4.75,2.75,3.75,1.75,3.5,4,1.75,4,3.25,3.75,2.25,2.25,4,2.5,5,3,0.75,3.25,4.25,3.25,4,1.5,3,5.5,2,3.5,2.5,3.5,6,1.5,2,4.25,5.75,3.5,4.75,5,2,3,4.5,2.5,1.25,3,3.25,2.25,6.5,1.75,3,2,3.5,3.5,1,4,2.25,2.5,0.75,2.5,3.25,2,3.5,3.5,2.75,3.25,4.5,5,2.75,4,3.25,1.75,4.25,2.75,6.5,6.25,4.75,2.75,4.75,2.5,1.5,2.25,2.75,4,2,4.5,3.25,3.75,4,3.5,2,3.5,6.25,5,3.25,4.25,0.5,2.75,5.25,2.5,3.5,4.5,1.25,1.25,4.5,2.5,7,2.25],"text":["Age: 25.038<br />Concealing: 0.75","Age: 25.873<br />Concealing: 4.50","Age: 22.199<br />Concealing: 1.50","Age: 27.259<br />Concealing: 3.75","Age: 49.022<br />Concealing: 2.00","Age: 48.811<br />Concealing: 1.75","Age: 20.214<br />Concealing: 4.25","Age: 22.580<br />Concealing: 6.75","Age: 30.164<br />Concealing: 6.00","Age: 23.179<br />Concealing: 1.00","Age: 24.496<br />Concealing: 2.75","Age: 26.273<br />Concealing: 5.00","Age: 19.286<br />Concealing: 2.75","Age: 25.014<br />Concealing: 3.25","Age: 52.508<br />Concealing: 0.50","Age: 23.179<br />Concealing: 5.75","Age: 25.129<br />Concealing: 5.25","Age: 31.587<br />Concealing: 2.00","Age: 24.072<br />Concealing: 2.25","Age: 20.855<br />Concealing: 6.50","Age: 21.884<br />Concealing: 3.50","Age: 24.395<br />Concealing: 4.25","Age: 22.758<br />Concealing: 4.00","Age: 21.462<br />Concealing: 3.00","Age: 57.239<br />Concealing: 1.50","Age: 22.418<br />Concealing: 4.00","Age: 23.729<br />Concealing: 3.50","Age: 21.109<br />Concealing: 4.50","Age: 21.942<br />Concealing: 3.75","Age: 23.029<br />Concealing: 2.00","Age: 21.337<br />Concealing: 2.00","Age: 22.117<br />Concealing: 5.00","Age: 22.763<br />Concealing: 6.25","Age: 23.387<br />Concealing: 4.25","Age: 24.595<br />Concealing: 3.75","Age: 22.330<br />Concealing: 6.00","Age: 22.298<br />Concealing: 0.75","Age: 23.264<br />Concealing: 5.25","Age: 25.030<br />Concealing: 4.50","Age: 20.069<br />Concealing: 3.25","Age: 21.356<br />Concealing: 4.75","Age: 38.689<br />Concealing: 4.50","Age: 22.623<br />Concealing: 2.75","Age: 22.355<br />Concealing: 2.25","Age: 19.072<br />Concealing: 5.50","Age: 23.948<br />Concealing: 2.75","Age: 21.753<br />Concealing: 4.75","Age: 21.605<br />Concealing: 3.00","Age: 21.246<br />Concealing: 6.25","Age: 23.149<br />Concealing: 2.75","Age: 22.530<br />Concealing: 1.50","Age: 18.944<br />Concealing: 3.50","Age: 21.660<br />Concealing: 1.50","Age: 52.636<br />Concealing: 3.25","Age: 40.127<br />Concealing: 4.25","Age: 22.330<br />Concealing: 0.75","Age: 23.650<br />Concealing: 6.50","Age: 20.942<br />Concealing: 3.50","Age: 22.467<br />Concealing: 4.50","Age: 21.983<br />Concealing: 7.00","Age: 23.108<br />Concealing: 2.00","Age: 22.678<br />Concealing: 4.00","Age: 64.215<br />Concealing: 5.50","Age: 21.065<br />Concealing: 4.00","Age: 23.729<br />Concealing: 4.25","Age: 27.667<br />Concealing: 1.25","Age: 18.897<br />Concealing: 4.00","Age: 21.408<br />Concealing: 3.50","Age: 21.101<br />Concealing: 5.00","Age: 29.255<br />Concealing: 0.25","Age: 21.410<br />Concealing: 4.75","Age: 20.964<br />Concealing: 1.50","Age: 21.172<br />Concealing: 5.00","Age: 24.909<br />Concealing: 5.25","Age: 21.646<br />Concealing: 6.25","Age: 20.822<br />Concealing: 3.50","Age: 26.796<br />Concealing: 2.50","Age: 22.659<br />Concealing: 2.50","Age: 24.493<br />Concealing: 1.25","Age: 18.276<br />Concealing: 3.50","Age: 23.127<br />Concealing: 6.25","Age: 19.683<br />Concealing: 3.25","Age: 48.445<br />Concealing: 4.75","Age: 24.370<br />Concealing: 4.75","Age: 20.688<br />Concealing: 1.25","Age: 20.532<br />Concealing: 3.25","Age: 25.014<br />Concealing: 4.25","Age: 21.386<br />Concealing: 6.25","Age: 24.006<br />Concealing: 1.50","Age: 22.777<br />Concealing: 5.25","Age: 22.689<br />Concealing: 4.00","Age: 23.792<br />Concealing: 5.75","Age: 21.624<br />Concealing: 5.25","Age: 28.061<br />Concealing: 1.25","Age: 26.963<br />Concealing: 2.25","Age: 21.087<br />Concealing: 3.00","Age: 37.860<br />Concealing: 4.00","Age: 20.373<br />Concealing: 4.00","Age: 21.156<br />Concealing: 3.50","Age: 23.001<br />Concealing: 6.75","Age: 21.194<br />Concealing: 0.75","Age: 22.530<br />Concealing: 5.75","Age: 22.473<br />Concealing: 2.75","Age: 31.459<br />Concealing: 4.25","Age: 21.536<br />Concealing: 5.00","Age: 67.144<br />Concealing: 3.25","Age: 19.817<br />Concealing: 2.75","Age: 24.704<br />Concealing: 5.00","Age: 26.456<br />Concealing: 3.75","Age: 21.922<br />Concealing: 3.00","Age: 22.506<br />Concealing: 4.75","Age: 22.626<br />Concealing: 2.50","Age: 26.758<br />Concealing: 3.50","Age: 22.141<br />Concealing: 2.75","Age: 23.094<br />Concealing: 1.75","Age: 21.227<br />Concealing: 2.75","Age: 23.779<br />Concealing: 3.25","Age: 26.566<br />Concealing: 4.00","Age: 23.119<br />Concealing: 3.50","Age: 19.223<br />Concealing: 3.50","Age: 25.449<br />Concealing: 1.25","Age: 23.647<br />Concealing: 0.00","Age: 22.966<br />Concealing: 4.25","Age: 24.671<br />Concealing: 3.00","Age: 22.215<br />Concealing: 5.50","Age: 21.849<br />Concealing: 3.50","Age: 58.274<br />Concealing: 4.75","Age: 25.287<br />Concealing: 2.75","Age: 21.315<br />Concealing: 2.00","Age: 35.579<br />Concealing: 4.00","Age: 24.241<br />Concealing: 3.25","Age: 22.960<br />Concealing: 5.00","Age: 22.810<br />Concealing: 3.50","Age: 24.458<br />Concealing: 1.75","Age: 19.294<br />Concealing: 2.75","Age: 21.832<br />Concealing: 4.25","Age: 18.900<br />Concealing: 4.50","Age: 20.011<br />Concealing: 1.75","Age: 19.664<br />Concealing: 4.25","Age: 21.583<br />Concealing: 3.50","Age: 19.650<br />Concealing: 2.75","Age: 22.339<br />Concealing: 5.75","Age: 21.312<br />Concealing: 2.00","Age: 21.734<br />Concealing: 2.25","Age: 30.615<br />Concealing: 6.50","Age: 22.314<br />Concealing: 4.50","Age: 24.589<br />Concealing: 1.50","Age: 23.458<br />Concealing: 2.50","Age: 22.867<br />Concealing: 6.00","Age: 19.376<br />Concealing: 0.50","Age: 22.618<br />Concealing: 5.25","Age: 21.613<br />Concealing: 3.00","Age: 23.324<br />Concealing: 3.25","Age: 25.331<br />Concealing: 5.25","Age: 19.658<br />Concealing: 2.25","Age: 22.643<br />Concealing: 2.50","Age: 19.957<br />Concealing: 5.25","Age: 45.822<br />Concealing: 5.00","Age: 23.428<br />Concealing: 4.00","Age: 23.261<br />Concealing: 4.50","Age: 19.319<br />Concealing: 6.00","Age: 20.496<br />Concealing: 4.00","Age: 19.757<br />Concealing: 4.25","Age: 21.175<br />Concealing: 1.75","Age: 19.519<br />Concealing: 1.25","Age: 23.888<br />Concealing: 2.75","Age: 22.284<br />Concealing: 4.50","Age: 21.769<br />Concealing: 3.00","Age: 22.478<br />Concealing: 3.25","Age: 24.480<br />Concealing: 4.75","Age: 40.236<br />Concealing: 1.75","Age: 21.230<br />Concealing: 3.75","Age: 21.405<br />Concealing: 4.50","Age: 21.714<br />Concealing: 3.50","Age: 21.085<br />Concealing: 3.75","Age: 21.933<br />Concealing: 6.75","Age: 20.460<br />Concealing: 1.50","Age: 19.587<br />Concealing: 6.00","Age: 18.788<br />Concealing: 4.25","Age: 21.525<br />Concealing: 6.00","Age: 32.540<br />Concealing: 3.50","Age: 22.437<br />Concealing: 6.25","Age: 24.805<br />Concealing: 4.50","Age: 21.306<br />Concealing: 2.75","Age: 21.435<br />Concealing: 4.25","Age: 22.308<br />Concealing: 5.25","Age: 40.915<br />Concealing: 3.75","Age: 20.406<br />Concealing: 3.50","Age: 21.632<br />Concealing: 4.00","Age: 20.855<br />Concealing: 3.75","Age: 23.508<br />Concealing: 4.50","Age: 24.480<br />Concealing: 5.25","Age: 21.734<br />Concealing: 3.50","Age: 19.959<br />Concealing: 5.75","Age: 30.996<br />Concealing: 2.00","Age: 18.221<br />Concealing: 3.00","Age: 22.150<br />Concealing: 1.25","Age: 24.800<br />Concealing: 5.00","Age: 22.013<br />Concealing: 3.25","Age: 20.866<br />Concealing: 2.75","Age: 25.369<br />Concealing: 4.50","Age: 20.351<br />Concealing: 6.50","Age: 22.883<br />Concealing: 1.75","Age: 23.050<br />Concealing: 4.50","Age: 20.148<br />Concealing: 5.50","Age: 22.393<br />Concealing: 5.25","Age: 30.519<br />Concealing: 6.25","Age: 21.605<br />Concealing: 4.00","Age: 23.612<br />Concealing: 5.25","Age: 21.430<br />Concealing: 5.25","Age: 29.109<br />Concealing: 6.00","Age: 23.294<br />Concealing: 4.50","Age: 21.482<br />Concealing: 1.00","Age: 21.958<br />Concealing: 4.50","Age: 20.972<br />Concealing: 3.50","Age: 21.649<br />Concealing: 3.50","Age: 21.972<br />Concealing: 6.00","Age: 20.458<br />Concealing: 3.75","Age: 24.014<br />Concealing: 3.00","Age: 21.211<br />Concealing: 4.75","Age: 23.549<br />Concealing: 4.50","Age: 22.525<br />Concealing: 5.75","Age: 21.788<br />Concealing: 3.75","Age: 19.560<br />Concealing: 1.00","Age: 25.435<br />Concealing: 0.75","Age: 23.647<br />Concealing: 4.50","Age: 22.240<br />Concealing: 3.75","Age: 18.952<br />Concealing: 3.25","Age: 23.458<br />Concealing: 1.00","Age: 21.243<br />Concealing: 4.25","Age: 24.239<br />Concealing: 5.50","Age: 21.274<br />Concealing: 4.00","Age: 22.590<br />Concealing: 7.00","Age: 37.080<br />Concealing: 5.25","Age: 21.454<br />Concealing: 3.75","Age: 19.686<br />Concealing: 4.25","Age: 46.895<br />Concealing: 3.25","Age: 22.185<br />Concealing: 2.25","Age: 19.891<br />Concealing: 2.50","Age: 22.139<br />Concealing: 4.00","Age: 19.239<br />Concealing: 2.75","Age: 21.460<br />Concealing: 2.50","Age: 21.361<br />Concealing: 3.00","Age: 52.026<br />Concealing: 3.00","Age: 25.621<br />Concealing: 2.50","Age: 36.970<br />Concealing: 5.00","Age: 24.496<br />Concealing: 3.75","Age: 19.806<br />Concealing: 4.25","Age: 42.895<br />Concealing: 1.75","Age: 21.290<br />Concealing: 5.00","Age: 21.479<br />Concealing: 2.75","Age: 21.087<br />Concealing: 4.75","Age: 21.884<br />Concealing: 1.75","Age: 20.299<br />Concealing: 0.25","Age: 20.912<br />Concealing: 4.25","Age: 21.859<br />Concealing: 3.75","Age: 22.552<br />Concealing: 6.25","Age: 23.431<br />Concealing: 3.25","Age: 21.216<br />Concealing: 2.50","Age: 22.415<br />Concealing: 5.00","Age: 20.055<br />Concealing: 3.00","Age: 24.228<br />Concealing: 4.00","Age: 22.872<br />Concealing: 3.00","Age: 23.081<br />Concealing: 4.75","Age: 21.701<br />Concealing: 2.50","Age: 26.295<br />Concealing: 1.50","Age: 26.391<br />Concealing: 5.00","Age: 21.654<br />Concealing: 4.00","Age: 23.617<br />Concealing: 3.75","Age: 25.610<br />Concealing: 3.50","Age: 20.690<br />Concealing: 5.75","Age: 23.817<br />Concealing: 3.00","Age: 25.085<br />Concealing: 2.75","Age: 21.109<br />Concealing: 4.25","Age: 29.244<br />Concealing: 2.00","Age: 32.891<br />Concealing: 5.00","Age: 21.602<br />Concealing: 3.75","Age: 22.174<br />Concealing: 3.50","Age: 23.656<br />Concealing: 2.25","Age: 22.670<br />Concealing: 4.00","Age: 23.193<br />Concealing: 6.75","Age: 21.558<br />Concealing: 5.25","Age: 44.245<br />Concealing: 1.00","Age: 27.951<br />Concealing: 2.00","Age: 27.212<br />Concealing: 2.50","Age: 26.875<br />Concealing: 3.50","Age: 30.555<br />Concealing: 4.75","Age: 21.920<br />Concealing: 1.50","Age: 31.292<br />Concealing: 4.75","Age: 28.436<br />Concealing: 4.00","Age: 23.130<br />Concealing: 2.75","Age: 40.206<br />Concealing: 2.00","Age: 33.830<br />Concealing: 3.75","Age: 23.105<br />Concealing: 3.50","Age: 33.055<br />Concealing: 4.25","Age: 20.773<br />Concealing: 4.00","Age: 21.232<br />Concealing: 2.50","Age: 27.220<br />Concealing: 3.25","Age: 19.313<br />Concealing: 4.25","Age: 19.669<br />Concealing: 5.50","Age: 42.323<br />Concealing: 3.50","Age: 29.205<br />Concealing: 4.75","Age: 19.321<br />Concealing: 3.50","Age: 25.512<br />Concealing: 4.25","Age: 22.024<br />Concealing: 5.50","Age: 21.367<br />Concealing: 3.25","Age: 19.190<br />Concealing: 4.25","Age: 28.033<br />Concealing: 4.25","Age: 22.632<br />Concealing: 5.75","Age: 24.838<br />Concealing: 3.75","Age: 20.066<br />Concealing: 2.50","Age: 19.004<br />Concealing: 4.75","Age: 18.377<br />Concealing: 5.00","Age: 22.057<br />Concealing: 1.75","Age: 24.822<br />Concealing: 2.25","Age: 22.736<br />Concealing: 3.00","Age: 74.561<br />Concealing: 4.25","Age: 23.497<br />Concealing: 4.25","Age: 25.153<br />Concealing: 0.75","Age: 22.711<br />Concealing: 6.00","Age: 25.780<br />Concealing: 7.00","Age: 57.855<br />Concealing: 4.25","Age: 45.165<br />Concealing: 3.25","Age: 27.924<br />Concealing: 4.25","Age: 20.863<br />Concealing: 2.25","Age: 21.331<br />Concealing: 2.75","Age: 23.784<br />Concealing: 4.00","Age: 23.557<br />Concealing: 3.00","Age: 20.504<br />Concealing: 6.25","Age: 20.438<br />Concealing: 5.50","Age: 31.582<br />Concealing: 3.75","Age: 24.696<br />Concealing: 3.75","Age: 23.297<br />Concealing: 3.00","Age: 18.935<br />Concealing: 3.25","Age: 21.345<br />Concealing: 3.50","Age: 25.145<br />Concealing: 3.50","Age: 20.334<br />Concealing: 5.75","Age: 21.542<br />Concealing: 4.75","Age: 24.085<br />Concealing: 4.75","Age: 18.495<br />Concealing: 4.25","Age: 70.318<br />Concealing: 5.00","Age: 21.712<br />Concealing: 3.75","Age: 19.831<br />Concealing: 4.75","Age: 23.995<br />Concealing: 4.50","Age: 23.097<br />Concealing: 3.75","Age: 21.898<br />Concealing: 4.75","Age: 25.449<br />Concealing: 3.25","Age: 20.230<br />Concealing: 5.50","Age: 22.563<br />Concealing: 2.00","Age: 18.536<br />Concealing: 3.50","Age: 21.917<br />Concealing: 3.50","Age: 19.434<br />Concealing: 3.75","Age: 22.590<br />Concealing: 3.50","Age: 23.656<br />Concealing: 5.25","Age: 22.585<br />Concealing: 5.50","Age: 19.743<br />Concealing: 3.25","Age: 19.768<br />Concealing: 4.00","Age: 22.774<br />Concealing: 2.25","Age: 22.508<br />Concealing: 3.00","Age: 21.432<br />Concealing: 4.25","Age: 22.278<br />Concealing: 2.75","Age: 28.061<br />Concealing: 4.00","Age: 23.187<br />Concealing: 3.00","Age: 24.762<br />Concealing: 3.75","Age: 21.265<br />Concealing: 4.50","Age: 20.556<br />Concealing: 1.75","Age: 29.378<br />Concealing: 5.75","Age: 48.683<br />Concealing: 4.25","Age: 19.190<br />Concealing: 3.25","Age: 18.971<br />Concealing: 3.50","Age: 20.455<br />Concealing: 1.75","Age: 30.640<br />Concealing: 6.75","Age: 20.647<br />Concealing: 4.75","Age: 20.135<br />Concealing: 4.25","Age: 22.361<br />Concealing: 3.75","Age: 21.561<br />Concealing: 6.50","Age: 19.948<br />Concealing: 1.25","Age: 19.094<br />Concealing: 4.75","Age: 30.268<br />Concealing: 2.75","Age: 25.421<br />Concealing: 6.75","Age: 19.469<br />Concealing: 2.75","Age: 22.306<br />Concealing: 3.00","Age: 23.719<br />Concealing: 4.00","Age: 30.216<br />Concealing: 2.50","Age: 22.226<br />Concealing: 4.25","Age: 25.958<br />Concealing: 2.75","Age: 19.220<br />Concealing: 2.00","Age: 19.584<br />Concealing: 2.25","Age: 18.859<br />Concealing: 2.00","Age: 32.480<br />Concealing: 4.75","Age: 22.826<br />Concealing: 5.00","Age: 21.134<br />Concealing: 5.25","Age: 22.653<br />Concealing: 4.50","Age: 33.534<br />Concealing: 5.25","Age: 26.090<br />Concealing: 2.00","Age: 21.966<br />Concealing: 4.50","Age: 23.250<br />Concealing: 4.25","Age: 19.842<br />Concealing: 5.25","Age: 27.543<br />Concealing: 5.00","Age: 20.269<br />Concealing: 2.75","Age: 28.482<br />Concealing: 1.25","Age: 19.267<br />Concealing: 2.00","Age: 23.825<br />Concealing: 4.00","Age: 18.922<br />Concealing: 1.25","Age: 20.723<br />Concealing: 5.75","Age: 21.736<br />Concealing: 4.00","Age: 20.575<br />Concealing: 1.00","Age: 22.582<br />Concealing: 1.25","Age: 18.355<br />Concealing: 2.50","Age: 31.483<br />Concealing: 2.50","Age: 20.214<br />Concealing: 4.00","Age: 20.534<br />Concealing: 2.00","Age: 25.224<br />Concealing: 2.75","Age: 21.849<br />Concealing: 1.25","Age: 18.558<br />Concealing: 3.75","Age: 23.409<br />Concealing: 6.25","Age: 21.408<br />Concealing: 5.25","Age: 21.339<br />Concealing: 6.00","Age: 28.827<br />Concealing: 3.00","Age: 21.238<br />Concealing: 4.00","Age: 21.879<br />Concealing: 5.25","Age: 23.656<br />Concealing: 2.00","Age: 31.409<br />Concealing: 4.00","Age: 19.371<br />Concealing: 3.75","Age: 22.109<br />Concealing: 4.50","Age: 25.498<br />Concealing: 3.50","Age: 22.026<br />Concealing: 2.75","Age: 23.379<br />Concealing: 5.25","Age: 18.021<br />Concealing: 7.00","Age: 19.467<br />Concealing: 5.00","Age: 25.520<br />Concealing: 3.00","Age: 21.290<br />Concealing: 6.25","Age: 25.766<br />Concealing: 6.75","Age: 20.603<br />Concealing: 4.75","Age: 39.749<br />Concealing: 2.50","Age: 19.976<br />Concealing: 5.00","Age: 22.640<br />Concealing: 3.25","Age: 27.094<br />Concealing: 3.25","Age: 23.144<br />Concealing: 5.50","Age: 33.504<br />Concealing: 2.50","Age: 20.334<br />Concealing: 5.00","Age: 24.345<br />Concealing: 1.50","Age: 24.017<br />Concealing: 3.00","Age: 61.436<br />Concealing: 2.00","Age: 19.256<br />Concealing: 5.00","Age: 23.245<br />Concealing: 4.75","Age: 70.000<br />Concealing: 5.25","Age: 20.466<br />Concealing: 2.75","Age: 25.931<br />Concealing: 1.50","Age: 20.274<br />Concealing: 4.25","Age: 21.301<br />Concealing: 4.00","Age: 19.360<br />Concealing: 4.50","Age: 22.100<br />Concealing: 3.25","Age: 20.419<br />Concealing: 4.25","Age: 21.914<br />Concealing: 3.00","Age: 50.460<br />Concealing: 5.75","Age: 20.904<br />Concealing: 5.00","Age: 20.699<br />Concealing: 3.50","Age: 20.953<br />Concealing: 2.50","Age: 20.436<br />Concealing: 1.50","Age: 40.203<br />Concealing: 1.75","Age: 19.579<br />Concealing: 5.50","Age: 21.646<br />Concealing: 4.00","Age: 26.916<br />Concealing: 3.75","Age: 19.354<br />Concealing: 5.25","Age: 18.470<br />Concealing: 3.25","Age: 22.662<br />Concealing: 2.75","Age: 20.540<br />Concealing: 2.00","Age: 21.137<br />Concealing: 4.25","Age: 30.415<br />Concealing: 4.00","Age: 26.700<br />Concealing: 1.75","Age: 22.292<br />Concealing: 4.25","Age: 20.211<br />Concealing: 2.75","Age: 20.909<br />Concealing: 3.00","Age: 21.405<br />Concealing: 4.00","Age: 19.037<br />Concealing: 3.25","Age: 58.583<br />Concealing: 2.25","Age: 19.428<br />Concealing: 4.75","Age: 22.692<br />Concealing: 4.00","Age: 22.771<br />Concealing: 2.25","Age: 23.215<br />Concealing: 1.25","Age: 18.078<br />Concealing: 4.75","Age: 18.070<br />Concealing: 6.50","Age: 28.244<br />Concealing: 4.25","Age: 24.214<br />Concealing: 4.00","Age: 18.454<br />Concealing: 4.50","Age: 19.924<br />Concealing: 4.25","Age: 21.416<br />Concealing: 2.50","Age: 21.512<br />Concealing: 3.00","Age: 20.543<br />Concealing: 2.50","Age: 18.987<br />Concealing: 3.00","Age: 20.063<br />Concealing: 4.00","Age: 19.888<br />Concealing: 4.00","Age: 21.372<br />Concealing: 1.75","Age: 19.379<br />Concealing: 4.25","Age: 19.102<br />Concealing: 4.00","Age: 29.112<br />Concealing: 2.00","Age: 22.314<br />Concealing: 5.50","Age: 58.526<br />Concealing: 3.25","Age: 19.798<br />Concealing: 3.25","Age: 51.823<br />Concealing: 4.50","Age: 21.068<br />Concealing: 6.00","Age: 19.174<br />Concealing: 3.75","Age: 24.370<br />Concealing: 2.75","Age: 19.620<br />Concealing: 1.50","Age: 18.095<br />Concealing: 4.25","Age: 25.235<br />Concealing: 3.00","Age: 29.008<br />Concealing: 0.75","Age: 24.170<br />Concealing: 5.00","Age: 18.401<br />Concealing: 5.75","Age: 22.350<br />Concealing: 4.25","Age: 46.112<br />Concealing: 3.50","Age: 18.002<br />Concealing: 3.75","Age: 20.285<br />Concealing: 1.00","Age: 21.906<br />Concealing: 5.00","Age: 22.569<br />Concealing: 2.00","Age: 23.839<br />Concealing: 5.75","Age: 18.136<br />Concealing: 1.75","Age: 19.801<br />Concealing: 1.00","Age: 20.975<br />Concealing: 4.75","Age: 19.267<br />Concealing: 4.75","Age: 20.901<br />Concealing: 5.50","Age: 22.656<br />Concealing: 3.75","Age: 21.599<br />Concealing: 7.00","Age: 22.089<br />Concealing: 5.00","Age: 38.106<br />Concealing: 3.75","Age: 27.190<br />Concealing: 3.50","Age: 22.517<br />Concealing: 6.00","Age: 30.402<br />Concealing: 2.50","Age: 24.332<br />Concealing: 5.75","Age: 22.232<br />Concealing: 2.50","Age: 19.836<br />Concealing: 3.75","Age: 18.445<br />Concealing: 4.00","Age: 21.525<br />Concealing: 3.25","Age: 24.329<br />Concealing: 4.00","Age: 19.447<br />Concealing: 4.25","Age: 18.897<br />Concealing: 6.25","Age: 19.434<br />Concealing: 4.25","Age: 35.095<br />Concealing: 5.50","Age: 22.210<br />Concealing: 3.25","Age: 21.309<br />Concealing: 4.00","Age: 33.249<br />Concealing: 6.75","Age: 20.039<br />Concealing: 2.75","Age: 19.874<br />Concealing: 2.25","Age: 19.379<br />Concealing: 4.00","Age: 21.870<br />Concealing: 2.50","Age: 18.308<br />Concealing: 3.25","Age: 18.733<br />Concealing: 4.00","Age: 33.170<br />Concealing: 6.25","Age: 19.231<br />Concealing: 3.25","Age: 49.011<br />Concealing: 3.75","Age: 27.532<br />Concealing: 5.75","Age: 47.432<br />Concealing: 3.75","Age: 21.317<br />Concealing: 5.00","Age: 22.555<br />Concealing: 4.50","Age: 44.494<br />Concealing: 3.50","Age: 22.051<br />Concealing: 5.00","Age: 19.623<br />Concealing: 6.00","Age: 18.147<br />Concealing: 4.00","Age: 19.245<br />Concealing: 4.50","Age: 19.447<br />Concealing: 6.00","Age: 19.185<br />Concealing: 3.50","Age: 19.710<br />Concealing: 6.75","Age: 22.081<br />Concealing: 4.25","Age: 25.517<br />Concealing: 2.75","Age: 20.014<br />Concealing: 3.50","Age: 28.485<br />Concealing: 2.00","Age: 20.003<br />Concealing: 2.25","Age: 22.188<br />Concealing: 6.25","Age: 20.625<br />Concealing: 2.50","Age: 18.812<br />Concealing: 2.50","Age: 21.219<br />Concealing: 0.50","Age: 66.862<br />Concealing: 5.75","Age: 28.001<br />Concealing: 4.75","Age: 32.242<br />Concealing: 2.00","Age: 18.722<br />Concealing: 2.50","Age: 22.429<br />Concealing: 3.50","Age: 20.241<br />Concealing: 7.00","Age: 21.556<br />Concealing: 5.25","Age: 20.036<br />Concealing: 6.00","Age: 21.755<br />Concealing: 4.25","Age: 19.412<br />Concealing: 4.00","Age: 62.602<br />Concealing: 4.25","Age: 23.059<br />Concealing: 1.75","Age: 23.478<br />Concealing: 4.50","Age: 20.762<br />Concealing: 5.50","Age: 53.247<br />Concealing: 2.75","Age: 23.275<br />Concealing: 2.25","Age: 18.256<br />Concealing: 0.75","Age: 18.511<br />Concealing: 4.25","Age: 25.838<br />Concealing: 6.25","Age: 19.053<br />Concealing: 2.00","Age: 56.595<br />Concealing: 1.25","Age: 25.881<br />Concealing: 4.25","Age: 40.264<br />Concealing: 5.25","Age: 20.324<br />Concealing: 3.50","Age: 19.601<br />Concealing: 4.75","Age: 19.289<br />Concealing: 2.50","Age: 18.886<br />Concealing: 3.75","Age: 25.627<br />Concealing: 4.00","Age: 22.084<br />Concealing: 3.00","Age: 22.084<br />Concealing: 4.00","Age: 20.261<br />Concealing: 3.00","Age: 19.428<br />Concealing: 2.75","Age: 19.261<br />Concealing: 6.25","Age: 50.955<br />Concealing: 2.75","Age: 20.477<br />Concealing: 4.00","Age: 28.819<br />Concealing: 3.50","Age: 60.645<br />Concealing: 3.50","Age: 21.547<br />Concealing: 5.00","Age: 19.456<br />Concealing: 4.50","Age: 80.136<br />Concealing: 6.25","Age: 31.847<br />Concealing: 4.25","Age: 36.655<br />Concealing: 2.25","Age: 41.802<br />Concealing: 1.75","Age: 18.708<br />Concealing: 5.00","Age: 18.922<br />Concealing: 3.25","Age: 22.352<br />Concealing: 1.00","Age: 29.036<br />Concealing: 1.00","Age: 52.877<br />Concealing: 4.50","Age: 25.306<br />Concealing: 5.50","Age: 19.165<br />Concealing: 6.50","Age: 29.884<br />Concealing: 4.00","Age: 19.371<br />Concealing: 2.50","Age: 19.683<br />Concealing: 2.50","Age: 23.152<br />Concealing: 5.75","Age: 20.348<br />Concealing: 4.75","Age: 19.727<br />Concealing: 3.75","Age: 29.003<br />Concealing: 3.25","Age: 24.340<br />Concealing: 6.25","Age: 25.463<br />Concealing: 4.25","Age: 24.162<br />Concealing: 1.75","Age: 20.986<br />Concealing: 2.00","Age: 21.482<br />Concealing: 3.00","Age: 24.710<br />Concealing: 5.25","Age: 21.257<br />Concealing: 4.50","Age: 40.247<br />Concealing: 3.25","Age: 27.431<br />Concealing: 4.25","Age: 50.602<br />Concealing: 4.00","Age: 25.066<br />Concealing: 4.25","Age: 24.784<br />Concealing: 4.50","Age: 21.964<br />Concealing: 6.50","Age: 20.430<br />Concealing: 7.00","Age: 21.635<br />Concealing: 4.25","Age: 23.291<br />Concealing: 5.00","Age: 25.334<br />Concealing: 1.25","Age: 22.380<br />Concealing: 2.25","Age: 26.779<br />Concealing: 1.75","Age: 19.425<br />Concealing: 0.75","Age: 25.734<br />Concealing: 2.00","Age: 23.374<br />Concealing: 5.00","Age: 19.179<br />Concealing: 3.75","Age: 34.881<br />Concealing: 0.75","Age: 23.886<br />Concealing: 4.25","Age: 24.425<br />Concealing: 4.00","Age: 25.851<br />Concealing: 4.75","Age: 21.813<br />Concealing: 3.75","Age: 28.324<br />Concealing: 3.25","Age: 18.924<br />Concealing: 2.50","Age: 22.612<br />Concealing: 7.00","Age: 36.499<br />Concealing: 3.25","Age: 19.250<br />Concealing: 3.00","Age: 23.628<br />Concealing: 4.00","Age: 26.886<br />Concealing: 5.50","Age: 34.259<br />Concealing: 3.25","Age: 20.055<br />Concealing: 6.00","Age: 20.934<br />Concealing: 5.25","Age: 58.011<br />Concealing: 4.75","Age: 21.684<br />Concealing: 4.75","Age: 19.258<br />Concealing: 4.75","Age: 19.642<br />Concealing: 1.75","Age: 25.085<br />Concealing: 4.50","Age: 21.777<br />Concealing: 3.75","Age: 23.346<br />Concealing: 7.00","Age: 54.931<br />Concealing: 4.00","Age: 21.879<br />Concealing: 4.75","Age: 20.033<br />Concealing: 3.75","Age: 40.475<br />Concealing: 3.25","Age: 22.782<br />Concealing: 6.75","Age: 19.456<br />Concealing: 6.75","Age: 21.246<br />Concealing: 5.25","Age: 21.279<br />Concealing: 3.50","Age: 29.725<br />Concealing: 5.25","Age: 31.308<br />Concealing: 2.75","Age: 42.531<br />Concealing: 3.50","Age: 21.706<br />Concealing: 3.00","Age: 26.878<br />Concealing: 6.25","Age: 22.350<br />Concealing: 3.50","Age: 20.748<br />Concealing: 4.75","Age: 20.931<br />Concealing: 2.75","Age: 21.873<br />Concealing: 2.25","Age: 66.077<br />Concealing: 1.25","Age: 18.985<br />Concealing: 3.25","Age: 70.342<br />Concealing: 4.75","Age: 28.702<br />Concealing: 3.50","Age: 19.343<br />Concealing: 2.00","Age: 18.503<br />Concealing: 4.25","Age: 19.100<br />Concealing: 3.25","Age: 30.725<br />Concealing: 4.75","Age: 20.367<br />Concealing: 6.75","Age: 55.610<br />Concealing: 4.00","Age: 19.757<br />Concealing: 4.00","Age: 20.365<br />Concealing: 3.00","Age: 60.155<br />Concealing: 2.50","Age: 55.139<br />Concealing: 5.50","Age: 37.153<br />Concealing: 4.25","Age: 48.045<br />Concealing: 3.25","Age: 39.464<br />Concealing: 3.75","Age: 21.717<br />Concealing: 7.00","Age: 42.227<br />Concealing: 5.00","Age: 27.097<br />Concealing: 2.75","Age: 20.055<br />Concealing: 7.00","Age: 23.875<br />Concealing: 5.75","Age: 27.973<br />Concealing: 2.00","Age: 23.820<br />Concealing: 2.25","Age: 60.439<br />Concealing: 5.75","Age: 19.483<br />Concealing: 4.75","Age: 20.189<br />Concealing: 1.00","Age: 32.039<br />Concealing: 1.25","Age: 20.975<br />Concealing: 3.00","Age: 20.058<br />Concealing: 3.75","Age: 19.680<br />Concealing: 4.00","Age: 22.886<br />Concealing: 4.00","Age: 52.480<br />Concealing: 2.25","Age: 21.309<br />Concealing: 3.00","Age: 26.131<br />Concealing: 3.25","Age: 19.576<br />Concealing: 3.50","Age: 21.126<br />Concealing: 1.75","Age: 19.724<br />Concealing: 3.75","Age: 23.639<br />Concealing: 2.00","Age: 18.139<br />Concealing: 3.00","Age: 20.868<br />Concealing: 6.25","Age: 19.546<br />Concealing: 6.00","Age: 22.840<br />Concealing: 3.25","Age: 32.499<br />Concealing: 0.25","Age: 23.100<br />Concealing: 4.25","Age: 20.230<br />Concealing: 4.25","Age: 20.997<br />Concealing: 4.00","Age: 26.684<br />Concealing: 3.00","Age: 23.297<br />Concealing: 2.75","Age: 25.731<br />Concealing: 0.00","Age: 44.310<br />Concealing: 4.75","Age: 23.590<br />Concealing: 3.00","Age: 36.814<br />Concealing: 2.25","Age: 48.806<br />Concealing: 2.25","Age: 18.807<br />Concealing: 4.75","Age: 60.650<br />Concealing: 4.75","Age: 66.764<br />Concealing: 5.75","Age: 24.272<br />Concealing: 6.00","Age: 21.449<br />Concealing: 6.50","Age: 22.122<br />Concealing: 2.25","Age: 49.967<br />Concealing: 4.75","Age: 32.748<br />Concealing: 2.50","Age: 55.930<br />Concealing: 2.25","Age: 21.096<br />Concealing: 3.25","Age: 27.806<br />Concealing: 4.75","Age: 20.009<br />Concealing: 2.00","Age: 45.436<br />Concealing: 2.00","Age: 20.655<br />Concealing: 2.25","Age: 47.971<br />Concealing: 6.25","Age: 21.567<br />Concealing: 6.00","Age: 35.795<br />Concealing: 3.00","Age: 57.384<br />Concealing: 0.75","Age: 26.864<br />Concealing: 4.75","Age: 19.195<br />Concealing: 2.50","Age: 19.634<br />Concealing: 6.00","Age: 24.203<br />Concealing: 3.00","Age: 44.710<br />Concealing: 1.50","Age: 21.419<br />Concealing: 3.00","Age: 21.950<br />Concealing: 5.00","Age: 19.965<br />Concealing: 6.00","Age: 21.616<br />Concealing: 4.50","Age: 20.269<br />Concealing: 2.25","Age: 21.178<br />Concealing: 4.50","Age: 22.793<br />Concealing: 5.50","Age: 23.699<br />Concealing: 1.50","Age: 21.597<br />Concealing: 4.75","Age: 24.146<br />Concealing: 3.25","Age: 23.538<br />Concealing: 5.50","Age: 22.377<br />Concealing: 3.50","Age: 49.775<br />Concealing: 5.00","Age: 23.921<br />Concealing: 3.00","Age: 22.700<br />Concealing: 5.50","Age: 29.594<br />Concealing: 4.50","Age: 21.139<br />Concealing: 2.50","Age: 22.361<br />Concealing: 2.00","Age: 50.942<br />Concealing: 2.00","Age: 20.789<br />Concealing: 2.50","Age: 22.306<br />Concealing: 4.25","Age: 22.229<br />Concealing: 5.25","Age: 20.559<br />Concealing: 4.50","Age: 19.792<br />Concealing: 5.75","Age: 21.884<br />Concealing: 5.75","Age: 23.280<br />Concealing: 2.75","Age: 22.292<br />Concealing: 5.75","Age: 22.886<br />Concealing: 2.50","Age: 20.699<br />Concealing: 2.25","Age: 24.091<br />Concealing: 6.00","Age: 23.636<br />Concealing: 2.50","Age: 50.468<br />Concealing: 0.75","Age: 33.550<br />Concealing: 3.75","Age: 18.473<br />Concealing: 5.75","Age: 20.518<br />Concealing: 4.00","Age: 23.228<br />Concealing: 2.00","Age: 26.111<br />Concealing: 1.75","Age: 42.670<br />Concealing: 4.75","Age: 23.751<br />Concealing: 4.00","Age: 29.892<br />Concealing: 1.75","Age: 23.765<br />Concealing: 4.50","Age: 20.668<br />Concealing: 1.25","Age: 21.276<br />Concealing: 4.00","Age: 36.154<br />Concealing: 3.50","Age: 18.894<br />Concealing: 3.50","Age: 23.970<br />Concealing: 5.25","Age: 26.637<br />Concealing: 2.00","Age: 25.933<br />Concealing: 1.25","Age: 23.683<br />Concealing: 5.00","Age: 44.324<br />Concealing: 3.50","Age: 20.033<br />Concealing: 3.25","Age: 25.287<br />Concealing: 5.25","Age: 50.942<br />Concealing: 2.00","Age: 20.638<br />Concealing: 5.00","Age: 20.874<br />Concealing: 3.25","Age: 36.502<br />Concealing: 1.25","Age: 22.952<br />Concealing: 3.50","Age: 30.478<br />Concealing: 2.75","Age: 20.764<br />Concealing: 2.50","Age: 28.978<br />Concealing: 6.50","Age: 62.148<br />Concealing: 2.25","Age: 30.358<br />Concealing: 1.50","Age: 23.858<br />Concealing: 3.75","Age: 21.194<br />Concealing: 3.00","Age: 21.320<br />Concealing: 5.50","Age: 31.229<br />Concealing: 5.00","Age: 31.732<br />Concealing: 1.75","Age: 22.081<br />Concealing: 4.50","Age: 22.878<br />Concealing: 4.00","Age: 19.694<br />Concealing: 1.50","Age: 38.832<br />Concealing: 3.75","Age: 25.178<br />Concealing: 5.25","Age: 19.190<br />Concealing: 3.75","Age: 19.070<br />Concealing: 4.50","Age: 22.758<br />Concealing: 4.50","Age: 21.520<br />Concealing: 3.50","Age: 38.558<br />Concealing: 5.50","Age: 21.386<br />Concealing: 3.75","Age: 29.003<br />Concealing: 1.25","Age: 29.493<br />Concealing: 6.00","Age: 23.992<br />Concealing: 4.75","Age: 20.496<br />Concealing: 3.50","Age: 21.687<br />Concealing: 0.75","Age: 22.925<br />Concealing: 1.50","Age: 28.006<br />Concealing: 4.50","Age: 23.426<br />Concealing: 4.25","Age: 19.907<br />Concealing: 0.75","Age: 19.948<br />Concealing: 4.75","Age: 21.758<br />Concealing: 3.50","Age: 23.223<br />Concealing: 2.75","Age: 18.363<br />Concealing: 6.25","Age: 18.579<br />Concealing: 1.25","Age: 22.092<br />Concealing: 5.00","Age: 21.386<br />Concealing: 4.25","Age: 49.512<br />Concealing: 4.50","Age: 22.005<br />Concealing: 1.00","Age: 20.096<br />Concealing: 6.75","Age: 25.093<br />Concealing: 5.00","Age: 55.415<br />Concealing: 5.25","Age: 28.863<br />Concealing: 2.50","Age: 21.703<br />Concealing: 5.00","Age: 21.939<br />Concealing: 2.50","Age: 20.679<br />Concealing: 5.00","Age: 26.738<br />Concealing: 5.75","Age: 22.818<br />Concealing: 3.00","Age: 23.034<br />Concealing: 3.25","Age: 24.121<br />Concealing: 4.75","Age: 32.458<br />Concealing: 6.25","Age: 28.830<br />Concealing: 4.25","Age: 33.156<br />Concealing: 5.25","Age: 22.152<br />Concealing: 4.25","Age: 21.293<br />Concealing: 4.50","Age: 52.820<br />Concealing: 2.50","Age: 22.604<br />Concealing: 3.50","Age: 48.609<br />Concealing: 1.75","Age: 18.815<br />Concealing: 5.50","Age: 29.676<br />Concealing: 5.25","Age: 28.762<br />Concealing: 0.00","Age: 29.345<br />Concealing: 6.50","Age: 24.510<br />Concealing: 4.75","Age: 25.123<br />Concealing: 6.25","Age: 23.604<br />Concealing: 2.75","Age: 23.404<br />Concealing: 4.50","Age: 24.567<br />Concealing: 5.00","Age: 44.354<br />Concealing: 3.00","Age: 27.918<br />Concealing: 2.50","Age: 24.077<br />Concealing: 2.75","Age: 22.653<br />Concealing: 6.25","Age: 33.909<br />Concealing: 3.75","Age: 49.874<br />Concealing: 4.25","Age: 23.820<br />Concealing: 3.75","Age: 31.645<br />Concealing: 3.50","Age: 23.513<br />Concealing: 4.25","Age: 20.271<br />Concealing: 4.50","Age: 22.369<br />Concealing: 5.75","Age: 26.936<br />Concealing: 1.75","Age: 27.825<br />Concealing: 4.00","Age: 53.992<br />Concealing: 2.75","Age: 26.027<br />Concealing: 1.75","Age: 19.089<br />Concealing: 1.75","Age: 24.707<br />Concealing: 2.75","Age: 30.396<br />Concealing: 4.50","Age: 20.422<br />Concealing: 3.00","Age: 18.829<br />Concealing: 2.50","Age: 22.883<br />Concealing: 2.50","Age: 20.094<br />Concealing: 3.00","Age: 28.970<br />Concealing: 3.25","Age: 21.972<br />Concealing: 2.00","Age: 20.219<br />Concealing: 4.00","Age: 27.434<br />Concealing: 5.50","Age: 22.325<br />Concealing: 3.25","Age: 23.510<br />Concealing: 4.50","Age: 24.022<br />Concealing: 6.50","Age: 24.789<br />Concealing: 4.00","Age: 18.840<br />Concealing: 5.75","Age: 25.134<br />Concealing: 4.50","Age: 23.497<br />Concealing: 5.50","Age: 26.591<br />Concealing: 6.50","Age: 31.278<br />Concealing: 2.50","Age: 26.508<br />Concealing: 3.50","Age: 24.877<br />Concealing: 3.25","Age: 21.003<br />Concealing: 2.00","Age: 25.840<br />Concealing: 2.50","Age: 32.028<br />Concealing: 3.25","Age: 32.559<br />Concealing: 4.25","Age: 34.051<br />Concealing: 2.00","Age: 21.484<br />Concealing: 4.75","Age: 21.290<br />Concealing: 5.25","Age: 24.181<br />Concealing: 4.75","Age: 29.600<br />Concealing: 5.25","Age: 21.783<br />Concealing: 4.50","Age: 32.567<br />Concealing: 0.75","Age: 20.559<br />Concealing: 4.25","Age: 18.254<br />Concealing: 3.00","Age: 20.094<br />Concealing: 4.25","Age: 31.762<br />Concealing: 5.25","Age: 25.345<br />Concealing: 2.75","Age: 39.464<br />Concealing: 4.00","Age: 20.907<br />Concealing: 6.00","Age: 20.022<br />Concealing: 4.75","Age: 19.253<br />Concealing: 4.75","Age: 18.944<br />Concealing: 2.50","Age: 23.182<br />Concealing: 1.25","Age: 31.543<br />Concealing: 2.50","Age: 25.254<br />Concealing: 2.75","Age: 23.217<br />Concealing: 3.25","Age: 19.937<br />Concealing: 6.50","Age: 25.380<br />Concealing: 5.25","Age: 41.731<br />Concealing: 4.50","Age: 28.012<br />Concealing: 5.50","Age: 30.615<br />Concealing: 4.00","Age: 46.947<br />Concealing: 5.75","Age: 20.425<br />Concealing: 3.75","Age: 22.281<br />Concealing: 5.00","Age: 22.640<br />Concealing: 4.50","Age: 21.397<br />Concealing: 5.00","Age: 25.630<br />Concealing: 2.50","Age: 35.910<br />Concealing: 4.50","Age: 66.939<br />Concealing: 3.25","Age: 20.972<br />Concealing: 3.25","Age: 25.846<br />Concealing: 6.75","Age: 20.395<br />Concealing: 6.00","Age: 44.483<br />Concealing: 3.25","Age: 29.224<br />Concealing: 4.00","Age: 23.212<br />Concealing: 3.75","Age: 50.720<br />Concealing: 3.25","Age: 55.563<br />Concealing: 3.75","Age: 57.381<br />Concealing: 3.75","Age: 23.990<br />Concealing: 2.25","Age: 21.944<br />Concealing: 2.00","Age: 19.595<br />Concealing: 2.00","Age: 31.749<br />Concealing: 4.75","Age: 21.870<br />Concealing: 3.50","Age: 24.225<br />Concealing: 5.00","Age: 36.776<br />Concealing: 6.50","Age: 23.587<br />Concealing: 1.75","Age: 20.044<br />Concealing: 6.00","Age: 25.756<br />Concealing: 0.75","Age: 29.980<br />Concealing: 3.00","Age: 33.112<br />Concealing: 4.00","Age: 30.519<br />Concealing: 3.25","Age: 21.545<br />Concealing: 3.50","Age: 22.933<br />Concealing: 2.25","Age: 29.192<br />Concealing: 4.00","Age: 22.878<br />Concealing: 5.25","Age: 23.237<br />Concealing: 2.50","Age: 22.407<br />Concealing: 5.50","Age: 20.367<br />Concealing: 4.50","Age: 25.378<br />Concealing: 1.75","Age: 24.937<br />Concealing: 4.75","Age: 21.999<br />Concealing: 1.75","Age: 24.430<br />Concealing: 2.75","Age: 27.067<br />Concealing: 1.75","Age: 39.700<br />Concealing: 3.25","Age: 28.940<br />Concealing: 4.00","Age: 21.517<br />Concealing: 4.00","Age: 71.199<br />Concealing: 5.25","Age: 20.200<br />Concealing: 3.75","Age: 42.635<br />Concealing: 0.50","Age: 28.332<br />Concealing: 2.50","Age: 23.724<br />Concealing: 5.50","Age: 50.818<br />Concealing: 6.25","Age: 33.167<br />Concealing: 4.00","Age: 26.027<br />Concealing: 5.00","Age: 55.897<br />Concealing: 1.75","Age: 25.747<br />Concealing: 5.25","Age: 18.971<br />Concealing: 3.25","Age: 56.081<br />Concealing: 2.25","Age: 26.667<br />Concealing: 0.75","Age: 24.482<br />Concealing: 5.50","Age: 23.133<br />Concealing: 4.75","Age: 21.484<br />Concealing: 4.50","Age: 21.484<br />Concealing: 4.50","Age: 20.252<br />Concealing: 0.50","Age: 22.733<br />Concealing: 6.75","Age: 58.605<br />Concealing: 3.25","Age: 18.694<br />Concealing: 6.25","Age: 20.512<br />Concealing: 5.00","Age: 22.289<br />Concealing: 2.75","Age: 31.757<br />Concealing: 3.25","Age: 19.124<br />Concealing: 4.00","Age: 40.532<br />Concealing: 5.75","Age: 42.903<br />Concealing: 3.75","Age: 32.776<br />Concealing: 3.75","Age: 26.536<br />Concealing: 5.25","Age: 19.124<br />Concealing: 4.00","Age: 24.526<br />Concealing: 2.75","Age: 22.998<br />Concealing: 5.50","Age: 24.559<br />Concealing: 6.25","Age: 26.842<br />Concealing: 2.50","Age: 22.339<br />Concealing: 4.00","Age: 29.460<br />Concealing: 6.50","Age: 19.420<br />Concealing: 2.50","Age: 25.980<br />Concealing: 2.50","Age: 59.782<br />Concealing: 2.00","Age: 24.332<br />Concealing: 3.50","Age: 48.716<br />Concealing: 4.25","Age: 22.018<br />Concealing: 4.50","Age: 25.813<br />Concealing: 6.75","Age: 26.155<br />Concealing: 1.25","Age: 20.318<br />Concealing: 2.50","Age: 58.148<br />Concealing: 2.50","Age: 29.323<br />Concealing: 1.75","Age: 30.645<br />Concealing: 3.50","Age: 27.576<br />Concealing: 2.25","Age: 28.660<br />Concealing: 4.75","Age: 23.699<br />Concealing: 5.25","Age: 21.191<br />Concealing: 4.25","Age: 32.384<br />Concealing: 2.50","Age: 26.429<br />Concealing: 4.50","Age: 18.894<br />Concealing: 5.25","Age: 24.239<br />Concealing: 4.25","Age: 54.509<br />Concealing: 3.25","Age: 62.443<br />Concealing: 5.00","Age: 20.449<br />Concealing: 5.50","Age: 23.891<br />Concealing: 4.25","Age: 27.568<br />Concealing: 2.50","Age: 28.513<br />Concealing: 6.00","Age: 22.845<br />Concealing: 4.00","Age: 49.731<br />Concealing: 4.50","Age: 22.517<br />Concealing: 3.50","Age: 37.487<br />Concealing: 5.25","Age: 19.406<br />Concealing: 4.25","Age: 35.382<br />Concealing: 3.00","Age: 24.909<br />Concealing: 3.00","Age: 25.186<br />Concealing: 3.75","Age: 20.553<br />Concealing: 4.00","Age: 21.416<br />Concealing: 6.00","Age: 33.126<br />Concealing: 4.00","Age: 38.249<br />Concealing: 2.50","Age: 70.063<br />Concealing: 5.00","Age: 61.510<br />Concealing: 7.00","Age: 19.149<br />Concealing: 2.50","Age: 35.270<br />Concealing: 4.75","Age: 30.279<br />Concealing: 4.25","Age: 26.396<br />Concealing: 2.50","Age: 61.214<br />Concealing: 5.50","Age: 22.618<br />Concealing: 3.50","Age: 20.770<br />Concealing: 0.00","Age: 23.729<br />Concealing: 5.75","Age: 58.416<br />Concealing: 3.00","Age: 24.909<br />Concealing: 2.75","Age: 27.483<br />Concealing: 6.75","Age: 25.334<br />Concealing: 5.75","Age: 27.798<br />Concealing: 4.75","Age: 33.033<br />Concealing: 4.75","Age: 22.336<br />Concealing: 3.75","Age: 20.950<br />Concealing: 5.00","Age: 45.657<br />Concealing: 4.25","Age: 67.692<br />Concealing: 2.25","Age: 24.548<br />Concealing: 0.25","Age: 22.533<br />Concealing: 4.50","Age: 51.716<br />Concealing: 4.25","Age: 43.451<br />Concealing: 4.50","Age: 51.196<br />Concealing: 5.25","Age: 41.101<br />Concealing: 6.25","Age: 22.270<br />Concealing: 4.50","Age: 33.392<br />Concealing: 0.50","Age: 22.530<br />Concealing: 5.00","Age: 21.019<br />Concealing: 5.00","Age: 30.725<br />Concealing: 7.00","Age: 23.921<br />Concealing: 5.25","Age: 36.390<br />Concealing: 1.25","Age: 43.856<br />Concealing: 6.25","Age: 22.224<br />Concealing: 2.25","Age: 56.083<br />Concealing: 3.25","Age: 40.193<br />Concealing: 3.00","Age: 67.385<br />Concealing: 1.75","Age: 62.509<br />Concealing: 4.50","Age: 38.287<br />Concealing: 2.75","Age: 20.376<br />Concealing: 4.50","Age: 37.830<br />Concealing: 2.25","Age: 44.631<br />Concealing: 3.00","Age: 68.459<br />Concealing: 2.25","Age: 60.242<br />Concealing: 4.00","Age: 20.324<br />Concealing: 4.00","Age: 37.301<br />Concealing: 3.50","Age: 21.367<br />Concealing: 4.25","Age: 26.240<br />Concealing: 3.75","Age: 24.918<br />Concealing: 4.00","Age: 24.189<br />Concealing: 3.50","Age: 59.615<br />Concealing: 4.25","Age: 26.569<br />Concealing: 3.75","Age: 24.699<br />Concealing: 3.25","Age: 22.070<br />Concealing: 3.75","Age: 28.162<br />Concealing: 4.25","Age: 19.989<br />Concealing: 1.25","Age: 31.519<br />Concealing: 3.25","Age: 54.493<br />Concealing: 5.00","Age: 66.630<br />Concealing: 2.75","Age: 58.608<br />Concealing: 1.00","Age: 61.228<br />Concealing: 4.00","Age: 21.205<br />Concealing: 4.00","Age: 23.267<br />Concealing: 2.00","Age: 34.544<br />Concealing: 3.50","Age: 25.509<br />Concealing: 4.25","Age: 59.662<br />Concealing: 1.00","Age: 20.047<br />Concealing: 1.75","Age: 29.408<br />Concealing: 2.50","Age: 21.408<br />Concealing: 4.75","Age: 50.372<br />Concealing: 2.75","Age: 27.702<br />Concealing: 3.75","Age: 24.151<br />Concealing: 1.75","Age: 34.933<br />Concealing: 3.50","Age: 22.974<br />Concealing: 4.00","Age: 21.742<br />Concealing: 1.75","Age: 34.221<br />Concealing: 4.00","Age: 23.817<br />Concealing: 3.25","Age: 20.589<br />Concealing: 3.75","Age: 36.006<br />Concealing: 2.25","Age: 23.376<br />Concealing: 2.25","Age: 22.032<br />Concealing: 4.00","Age: 24.398<br />Concealing: 2.50","Age: 22.653<br />Concealing: 5.00","Age: 22.549<br />Concealing: 3.00","Age: 24.847<br />Concealing: 0.75","Age: 21.082<br />Concealing: 3.25","Age: 22.612<br />Concealing: 4.25","Age: 23.743<br />Concealing: 3.25","Age: 20.551<br />Concealing: 4.00","Age: 23.261<br />Concealing: 1.50","Age: 22.697<br />Concealing: 3.00","Age: 21.186<br />Concealing: 5.50","Age: 34.837<br />Concealing: 2.00","Age: 22.388<br />Concealing: 3.50","Age: 18.265<br />Concealing: 2.50","Age: 55.621<br />Concealing: 3.50","Age: 22.555<br />Concealing: 6.00","Age: 22.648<br />Concealing: 1.50","Age: 23.067<br />Concealing: 2.00","Age: 23.932<br />Concealing: 4.25","Age: 21.744<br />Concealing: 5.75","Age: 22.136<br />Concealing: 3.50","Age: 28.096<br />Concealing: 4.75","Age: 22.021<br />Concealing: 5.00","Age: 62.348<br />Concealing: 2.00","Age: 33.695<br />Concealing: 3.00","Age: 32.458<br />Concealing: 4.50","Age: 22.473<br />Concealing: 2.50","Age: 25.531<br />Concealing: 1.25","Age: 30.550<br />Concealing: 3.00","Age: 24.126<br />Concealing: 3.25","Age: 23.554<br />Concealing: 2.25","Age: 27.609<br />Concealing: 6.50","Age: 27.346<br />Concealing: 1.75","Age: 35.962<br />Concealing: 3.00","Age: 33.802<br />Concealing: 2.00","Age: 23.708<br />Concealing: 3.50","Age: 62.893<br />Concealing: 3.50","Age: 33.684<br />Concealing: 1.00","Age: 25.558<br />Concealing: 4.00","Age: 23.327<br />Concealing: 2.25","Age: 22.114<br />Concealing: 2.50","Age: 23.365<br />Concealing: 0.75","Age: 22.475<br />Concealing: 2.50","Age: 32.904<br />Concealing: 3.25","Age: 23.891<br />Concealing: 2.00","Age: 21.961<br />Concealing: 3.50","Age: 23.562<br />Concealing: 3.50","Age: 25.104<br />Concealing: 2.75","Age: 22.418<br />Concealing: 3.25","Age: 24.236<br />Concealing: 4.50","Age: 21.706<br />Concealing: 5.00","Age: 24.912<br />Concealing: 2.75","Age: 23.308<br />Concealing: 4.00","Age: 19.795<br />Concealing: 3.25","Age: 24.562<br />Concealing: 1.75","Age: 42.670<br />Concealing: 4.25","Age: 23.168<br />Concealing: 2.75","Age: 22.656<br />Concealing: 6.50","Age: 20.200<br />Concealing: 6.25","Age: 18.549<br />Concealing: 4.75","Age: 29.819<br />Concealing: 2.75","Age: 24.135<br />Concealing: 4.75","Age: 24.808<br />Concealing: 2.50","Age: 22.519<br />Concealing: 1.50","Age: 23.220<br />Concealing: 2.25","Age: 23.951<br />Concealing: 2.75","Age: 30.331<br />Concealing: 4.00","Age: 41.794<br />Concealing: 2.00","Age: 22.768<br />Concealing: 4.50","Age: 22.705<br />Concealing: 3.25","Age: 33.537<br />Concealing: 3.75","Age: 18.281<br />Concealing: 4.00","Age: 21.783<br />Concealing: 3.50","Age: 22.785<br />Concealing: 2.00","Age: 24.422<br />Concealing: 3.50","Age: 22.601<br />Concealing: 6.25","Age: 37.542<br />Concealing: 5.00","Age: 22.467<br />Concealing: 3.25","Age: 23.694<br />Concealing: 4.25","Age: 19.833<br />Concealing: 0.50","Age: 33.082<br />Concealing: 2.75","Age: 21.936<br />Concealing: 5.25","Age: 22.612<br />Concealing: 2.50","Age: 41.772<br />Concealing: 3.50","Age: 45.531<br />Concealing: 4.50","Age: 23.478<br />Concealing: 1.25","Age: 23.666<br />Concealing: 1.25","Age: 25.044<br />Concealing: 4.50","Age: 25.386<br />Concealing: 2.50","Age: 27.242<br />Concealing: 7.00","Age: 42.350<br />Concealing: 2.25"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(0,0,0,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(0,0,0,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":28.3979521239795,"r":7.97011207970112,"b":45.0754116507541,"l":34.2714819427148},"font":{"color":"rgba(0,0,0,1)","family":"serif","size":15.9402241594022},"xaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[14.8953,83.2427],"ticktext":["20","40","60","80"],"tickvals":[20,40,60,80],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.98505603985056,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"serif","size":12.7521793275218},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.531340805313408,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":"Age","titlefont":{"color":"rgba(0,0,0,1)","family":"serif","size":15.9402241594022},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"type":"linear","autorange":false,"tickmode":"array","range":[-0.35,7.35],"ticktext":["0","2","4","6"],"tickvals":[0,2,4,6],"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.98505603985056,"tickwidth":0.398505603985056,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"serif","size":12.7521793275218},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.531340805313408,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":"Concealing_Median","titlefont":{"color":"rgba(0,0,0,1)","family":"serif","size":15.9402241594022},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":null,"bordercolor":null,"borderwidth":0,"font":{"color":"rgba(0,0,0,1)","family":"serif","size":17.2685761726858}},"hovermode":"closest"},"source":"A","attrs":{"151ac24d71953":{"x":{},"y":{},"type":"ggplotly"},"151ac69af6195":{"x":{},"ymin":{},"ymax":{}},"151ac25bb41d4":{"x":{},"y":{}},"151ac5e9e3bf7":{"x":{},"ymin":{},"ymax":{}},"151ac6b79502":{"x":{},"y":{}}},"cur_data":"151ac24d71953","visdat":{"151ac24d71953":["function (y) ","x"],"151ac69af6195":["function (y) ","x"],"151ac25bb41d4":["function (y) ","x"],"151ac5e9e3bf7":["function (y) ","x"],"151ac6b79502":["function (y) ","x"]},"config":{"modeBarButtonsToAdd":[{"name":"Collaborate","icon":{"width":1000,"ascent":500,"descent":-50,"path":"M487 375c7-10 9-23 5-36l-79-259c-3-12-11-23-22-31-11-8-22-12-35-12l-263 0c-15 0-29 5-43 15-13 10-23 23-28 37-5 13-5 25-1 37 0 0 0 3 1 7 1 5 1 8 1 11 0 2 0 4-1 6 0 3-1 5-1 6 1 2 2 4 3 6 1 2 2 4 4 6 2 3 4 5 5 7 5 7 9 16 13 26 4 10 7 19 9 26 0 2 0 5 0 9-1 4-1 6 0 8 0 2 2 5 4 8 3 3 5 5 5 7 4 6 8 15 12 26 4 11 7 19 7 26 1 1 0 4 0 9-1 4-1 7 0 8 1 2 3 5 6 8 4 4 6 6 6 7 4 5 8 13 13 24 4 11 7 20 7 28 1 1 0 4 0 7-1 3-1 6-1 7 0 2 1 4 3 6 1 1 3 4 5 6 2 3 3 5 5 6 1 2 3 5 4 9 2 3 3 7 5 10 1 3 2 6 4 10 2 4 4 7 6 9 2 3 4 5 7 7 3 2 7 3 11 3 3 0 8 0 13-1l0-1c7 2 12 2 14 2l218 0c14 0 25-5 32-16 8-10 10-23 6-37l-79-259c-7-22-13-37-20-43-7-7-19-10-37-10l-248 0c-5 0-9-2-11-5-2-3-2-7 0-12 4-13 18-20 41-20l264 0c5 0 10 2 16 5 5 3 8 6 10 11l85 282c2 5 2 10 2 17 7-3 13-7 17-13z m-304 0c-1-3-1-5 0-7 1-1 3-2 6-2l174 0c2 0 4 1 7 2 2 2 4 4 5 7l6 18c0 3 0 5-1 7-1 1-3 2-6 2l-173 0c-3 0-5-1-8-2-2-2-4-4-4-7z m-24-73c-1-3-1-5 0-7 2-2 3-2 6-2l174 0c2 0 5 0 7 2 3 2 4 4 5 7l6 18c1 2 0 5-1 6-1 2-3 3-5 3l-174 0c-3 0-5-1-7-3-3-1-4-4-5-6z"},"click":"function(gd) { \n        // is this being viewed in RStudio?\n        if (location.search == '?viewer_pane=1') {\n          alert('To learn about plotly for collaboration, visit:\\n https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html');\n        } else {\n          window.open('https://cpsievert.github.io/plotly_book/plot-ly-for-collaboration.html', '_blank');\n        }\n      }"}],"cloud":false},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1}},"base_url":"https://plot.ly"},"evals":["config.modeBarButtonsToAdd.0.click"],"jsHooks":{"render":[{"code":"function(el, x) { var ctConfig = crosstalk.var('plotlyCrosstalkOpts').set({\"on\":\"plotly_click\",\"persistent\":false,\"dynamic\":false,\"selectize\":false,\"opacityDim\":0.2,\"selected\":{\"opacity\":1}}); }","data":null}]}}</script><!--/html_preserve-->


**It's good to take a few steps back and look at the bigger picture :)**

<!-- But which one, between these two models, is "objectively" better? -->

<!-- ## Model Comparison and Selection -->

<!-- It is often interesting to know which model better fits the data. The frequentist approach provides several indices of goodness of fit (AIC, BIC, ...), for which the Bayesian framework has equivalents. These are obtained through **Leave-one-out cross-validation (LOO)** and are called **ELPD** (*expected log predictive density*) and **LOOIC** (*LOO information criterion*). The best model is the one with  the largest ELPD (smallest LOOIC). See the [official rstanarm documentation](https://CRAN.R-project.org/package=rstanarm/vignettes/rstanarm.html#step-3-criticize-the-model) as well as the [loo package](https://CRAN.R-project.org/package=loo/vignettes/loo2-example.html) for details. -->

<!-- ```{r message=FALSE, warning=FALSE, include=FALSE, results="hide"} -->
<!-- # Fit the models -->
<!-- fit_intercept_only <- rstanarm::stan_glm(Concealing ~ 1, data=df, cores=1, chains=1, seed=666) -->
<!-- fit_linear <- rstanarm::stan_glm(Concealing ~ Age, data=df, cores=1, chains=1, seed=666) -->
<!-- fit_poly <- rstanarm::stan_glm(Concealing ~ poly(Age, 2, raw=TRUE), data=df, cores=1, chains=1, seed=666) -->

<!-- # Compute the LOO validation -->
<!-- loo_intercept_only <- rstanarm::loo(fit_intercept_only, cores=1) -->
<!-- loo_linear <- rstanarm::loo(fit_linear, cores=1) -->
<!-- loo_poly <- rstanarm::loo(fit_poly, cores=1) -->

<!-- # Compare the models -->
<!-- comparison <- rstanarm::compare_models(loo_intercept_only, -->
<!--                                        loo_linear, -->
<!--                                        loo_poly) -->
<!-- print(comparison) -->
<!-- ``` -->
<!-- ```{r, message=FALSE, results="hide", eval=FALSE, warning=FALSE} -->
<!-- # Fit the models -->
<!-- fit_intercept_only <- rstanarm::stan_glm(Concealing ~ 1, data=df) -->
<!-- fit_linear <- rstanarm::stan_glm(Concealing ~ Age, data=df) -->
<!-- fit_poly <- rstanarm::stan_glm(Concealing ~ poly(Age, 2, raw=TRUE), data=df) -->

<!-- # Compute the LOO cross-validation -->
<!-- loo_intercept_only <- rstanarm::loo(fit_intercept_only) -->
<!-- loo_linear <- rstanarm::loo(fit_linear) -->
<!-- loo_poly <- rstanarm::loo(fit_poly) -->

<!-- # Compare the models -->
<!-- comparison <- rstanarm::compare_models(loo_intercept_only, -->
<!--                                        loo_linear, -->
<!--                                        loo_poly) -->
<!-- print(comparison) -->
<!-- ``` -->
<!-- ```{r echo=FALSE, message=FALSE, warning=FALSE} -->
<!-- kable(comparison, digits=1) -->
<!-- ``` -->

<!-- As we can see, the best model (largest ELPD and smallest LOOIC) is the polynomial model, followed by the linear model. The worst model appears to be the constant (intercept-only) model. -->



## Piors Specification

One of the interesting aspect of the Bayesian framework is the possibility of adding prior expectations about the effect, to help model fitting and increase accuracy in noisy data or small samples.


### Weakly informative priors

As you might have notice, we didn't specify any priors in the previous analyses. In fact, we let the algorithm define and set *weakly informative priors*, designed to provide moderate regularization and help stabilize computation, without biasing the effect direction. For example, a wealky informative prior, for a standardized predictor (with mean = 0 and SD = 1) could be a normal distribution with mean = 0 and SD = 1. This means that the effect of this predictor is expected to be equally probable in any direction (as the distribution is symmetric around 0), with probability being higher close to 0 and lower far from 0.

While this prior doesn't bias the direction of the Bayesian (MCMC) sampling, it suggests that having an effect of 100 (*i.e.*, located at 100 SD of the mean as our variables are standardized) is highly unprobable, and that an effect close to 0 is more probable.

To better play with priors, let's start by standardizing our dataframe.



```r
# Standardize (scale and center) the numeric variables
dfZ <- psycho::standardize(df)
```

Then, we can explicitly specify a weakly informative prior for all effects of the model.


```r
# Let's fit our model
fit <- rstanarm::stan_glm(Life_Satisfaction ~ Tolerating, 
                          data=dfZ,
                          prior=normal(location = 0, # Mean
                                       scale = 1, # SD
                                       autoscale=FALSE)) # Don't adjust scale automatically
```

Let's plot the prior (the expectation) against the posterior (the estimated effect) distribution.


```r
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
```

![](bayesian_files/figure-html/unnamed-chunk-49-1.png)<!-- -->


This plot is rather ugly, because our posterior is very precise (due to the large sample) compared to the prior. 

### Informative priors

Although the default priors tend to work well, prudent use of more informative priors is encouraged. It is important to underline that setting informative priors (**if realistic**), does not overbias the analysis. In other words, is only "directs" the sampling: if the data are highly informative about the parameter values (enough to overwhelm the prior), a prudent informative prior (even if oppositive to the observed effect) will yield similar results to a non-informative prior. **In other words, you can't change the results dramatically by tweaking the priors**. But as the amount of data and/or the signal-to-noise ratio decrease, using a more informative prior becomes increasingly important. Of course, if you see someone using a prior with mean = 42 and SD = 0.0001, you should look at his results with caution...

Anyway, see the [official rstanarm documentation](https://CRAN.R-project.org/package=rstanarm/vignettes/priors.html) for details.








## Advanced Visualization

### Plot all iterations

As Bayesian models usually generate a lot of samples (*iterations*), one could want to plot them as well, instead (or along) the posterior "summary". This can be done quite easily by extracting all the iterations in `get_predicted`.


```r
# Fit the model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = "binomial")
```

```r
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
```

<img src="bayesian_files/figure-html/unnamed-chunk-51-1.png" style="display: block; margin: auto;" />




## Credits

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

- Makowski, (2018). *The psycho Package: An Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. https://doi.org/10.21105/joss.00470

## Contribution

Improve this vignette by modifying [this](https://github.com/neuropsychology/psycho.R/blob/master/vignettes/bayesian.Rmd) file!
