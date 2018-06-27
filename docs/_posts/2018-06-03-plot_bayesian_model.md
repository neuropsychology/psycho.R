---
layout: post
title: "Fancy Plot (with Posterior Samples) for Bayesian Regressions"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-06-03
summary: Describes how to plot a Bayesian regression with all the draws from the posterior distribution.
---

-   [The Model](#the-model)
-   [Plot](#plot)
-   [Credits](#credits)

As Bayesian models usually generate a lot of samples (*iterations*), one could want to plot them as well, instead (or along) the posterior "summary" (with indices like the 90% HDI). This can be done quite easily by extracting all the iterations in `get_predicted` from the `psycho` package.

The Model
=========

``` r
# devtools::install_github("neuropsychology/psycho.R")  # Install the last psycho version if needed

# Load packages
library(tidyverse)
library(psycho)

# Import data
df <- psycho::affective

# Fit a logistic regression model
fit <- rstanarm::stan_glm(Sex ~ Adjusting, data=df, family = "binomial")
```

We fitted a Bayesian logistic regression to predict the sex (*W / M*) with one's ability to flexibly adjust to his/her emotional reaction.

Plot
====

To visualize the model, the most neat way is to extract a "reference grid" (*i.e.*, a theorethical dataframe with balanced data). Our refgrid is made of equally spaced predictor values. With it, we can make predictions using the previously fitted model. This will compute the median of the posterior prediction, as well as the 90% credible interval. However, we're interested in keeping all the prediction samples (iterations). Note that `get_predicted` automatically transformed log odds ratios (the values in which the model is expressed) to probabilities, easier to apprehend.

``` r
# Generate a new refgrid
refgrid <- df %>% 
  dplyr::select(Adjusting) %>% 
  psycho::refdata(length.out=10)

# Get predictions and keep iterations
predicted <- psycho::get_predicted(fit, newdata=refgrid, keep_iterations=TRUE)

# Reshape this dataframe to have iterations as factor
predicted <- predicted %>% 
  tidyr::gather(Iteration, Iteration_Value, starts_with("iter"))

# Plot all iterations with the median prediction
ggplot(predicted, aes(x=Adjusting)) +
  geom_line(aes(y=Iteration_Value, group=Iteration), size=0.3, alpha=0.01) +
  geom_line(aes(y=Sex_Median), size=1) + 
  ylab("Probability of being a man\n") +
  theme_classic()
```

<img src="https://raw.githubusercontent.com/neuropsychology/psycho.R/master/docs/_posts/2018-06-03-plot_bayesian_model_files/figure-markdown_github/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

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
