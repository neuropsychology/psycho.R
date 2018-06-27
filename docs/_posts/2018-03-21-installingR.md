---
layout: post
title: "Installing R and R studio"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-03-21
summary: Installing R and R studio
---

Guide
------------

### Install R and R Studio

-   Go here: <https://cloud.r-project.org/>
-   Download the last version for your OS
-   Install it
-   Go here: <https://www.rstudio.com/products/rstudio/download/#download>
-   Download the right version for your OS
-   Install it
-   Start R studio

### Install the psycho package

If you've never used `psycho`, enter one of the following in the console and press enter:

``` r
# This for the stable version:
install.packages("psycho")

# Or this for the dev version:
install.packages("devtools")
library(devtools)
devtools::install_github("neuropsychology/psycho.R")
```

Else, just put this at the beginning of every script:

``` r
library(psycho)
```


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
