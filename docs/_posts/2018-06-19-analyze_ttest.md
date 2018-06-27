---
layout: post
title: "Copy/paste t-tests Directly to Manuscripts"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-06-19
summary: APA formatting for t-tests.
---

-   [Do a t-test](#do-a-t-test)
-   [APA formatted output](#apa-formatted-output)
-   [Flexibility](#flexibility)
-   [Dataframe of Values](#dataframe-of-values)
-   [Contribute](#contribute)
-   [Credits](#credits)
-   [Previous blogposts](#previous-blogposts)

![](https://raw.githubusercontent.com/neuropsychology/psycho.R/master/vignettes/images/logo.PNG)

One of the most time-consuming part of data analysis in psychology is the copy-pasting of specific values of some R output to a manuscript or a report. This task is frustrating, prone to errors, and increases the variability of statistical reporting. This is an important issue, as standardizing practices of what and how to report might be a key to overcome the reproducibility crisis of psychology. **The [psycho](https://github.com/neuropsychology/psycho.R) package was designed specifically to do this job**. At first, for complex [Bayesian mixed models](https://cran.r-project.org/web/packages/psycho/vignettes/bayesian.html), but the package is now compatible with basic methods, such as **t-tests**.

Do a t-test
===========

``` r
# Load packages
library(tidyverse)

# devtools::install_github("neuropsychology/psycho.R")  # Install the latest psycho version
library(psycho)

df <- psycho::affective  # Load the data


results <- t.test(df$Age ~ df$Sex)  # Perform a simple t-test
```

APA formatted output
====================

You simply run the `analyze()` function on the t-test object.

``` r
psycho::analyze(results)
```

    The Welch Two Sample t-test suggests that the difference of df$Age by df$Sex (mean in group F = 26.78, mean in group M = 27.45, difference = -0.67) is not significant (t(360.68) = -0.86, 95% CI [-2.21, 0.87], p > .1).

Flexibility
===========

It works for all kinds of different t-tests versions.

``` r
t.test(df$Adjusting ~ df$Sex,
       var.equal=TRUE, 
       conf.level = .90) %>% 
  psycho::analyze()
```

    The  Two Sample t-test suggests that the difference of df$Adjusting by df$Sex (mean in group F = 3.72, mean in group M = 4.13, difference = -0.41) is significant (t(1249) = -4.13, 90% CI [-0.58, -0.25], p < .001).

``` r
t.test(df$Adjusting,
       mu=0,
       conf.level = .90) %>% 
  psycho::analyze()
```

    The One Sample t-test suggests that the difference between df$Adjusting (mean = 3.80) and mu = 0 is significant (t(1250) = 93.93, 90% CI [3.74, 3.87], p < .001).

Dataframe of Values
===================

It is also possible to have all the values stored in a dataframe by running a **summary** on the analyzed object.

``` r
library(tidyverse)

t.test(df$Adjusting ~ df$Sex) %>% 
  psycho::analyze() %>% 
  summary()
```

|      effect|  statistic|        df|        p|   CI\_lower|  CI\_higher|
|-----------:|----------:|---------:|--------:|-----------:|-----------:|
|  -0.4149661|  -4.067008|  377.8364|  5.8e-05|  -0.6155884|  -0.2143439|

Contribute
==========

Of course, these reporting standards should change, depending on new expert recommandations or official guidelines. **The goal of this package is to flexibly adapt to new changes and accompany the evolution of best practices**. Therefore, if you have any advices, opinions or ideas, we encourage you to let us know by opening an [issue](https://github.com/neuropsychology/psycho.R/issues) or, even better, to try to implement changes yourself by [contributing](https://github.com/neuropsychology/psycho.R/blob/master/.github/CONTRIBUTING.md) to the code.

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: An Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>

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