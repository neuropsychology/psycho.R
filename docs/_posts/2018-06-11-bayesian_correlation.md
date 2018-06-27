---
layout: post
title: "Easy APA Formatted Bayesian Correlation"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-06-11
summary: Perform, interpret and format Bayesian correlations in one line.
---



-   [Traditional Correlation](#traditional-correlation)
-   [Bayesian APA formatted Correlation](#bayesian-apa-formatted-correlation)
-   [Indices](#indices)
-   [Posterior](#posterior)
-   [Credits](#credits)


**The Bayesian framework is the right way to go for psychological science.** To facilitate its use for newcommers, we implemented the `bayes_cor.test` function in the [psycho package](https://github.com/neuropsychology/psycho.R), a **user-friendly wrapper** for the `correlationBF` function of the great [`BayesFactor`](https://richarddmorey.github.io/BayesFactor/) package by Richard D. Morey.

Traditional Correlation
=======================

Let's first perform a traditional correlation.

``` r
# devtools::install_github("neuropsychology/psycho.R")  # Install the latest psycho version

# Load packages
library(tidyverse)
library(psycho)

# Import data
df <- psycho::affective

cor.test(df$Concealing, df$Tolerating)
```


        Pearson's product-moment correlation

    data:  df$Concealing and df$Tolerating
    t = 2.611, df = 1249, p-value = 0.009136
    alternative hypothesis: true correlation is not equal to 0
    95 percent confidence interval:
     0.01832974 0.12857724
    sample estimates:
           cor 
    0.07367859 

There is a **significant** (*whatever that means*), yet **weak positive** correlation between Concealing and Tolerating affective styles.

Bayesian APA formatted Correlation
==================================

And now, see how quickly we can do a Bayesian correlation:

``` r
bayes_cor.test(df$Concealing, df$Tolerating)
```

    Results of the Bayesian correlation indicate anecdotal evidence (BF = 1.95) in favour of a positive association between df$Concealing and df$Tolerating (r = 0.073, MAD = 0.028, 90% CI [0.023, 0.12]). The correlation can be considered as small or very small with respective probabilities of 16.55% and 83.06%.

The results are roughly the same, but neatly dissociate the likelihood in favour or against the null hypothesis (using the [Bayes Factor](https://www.r-bloggers.com/what-does-a-bayes-factor-feel-like/)), from the "significance" (wether a portion of the *Credible Interval* covers 0), from the effect size (interpreted here with [Cohen's (1988) rules of thumb](https://github.com/neuropsychology/psycho.R/blob/master/R/interpret_r.R#L142)). Critically, **you can, now, simply copy/paste this output to your manuscript!** It includes and formats the Bayes Factor, the median (a good point-estimate, close to the *r* estimated in frequentist correlation), MAD (robust equivalent of SD for median) and *credible* interval (CI) of the posterior distribution, as well as effect size interpretation.

Indices
=======

We can have access to more indices with the `summary`:

``` r
results <- bayes_cor.test(df$Concealing, df$Tolerating)
summary(results)
```

|                                                   |  Median|   MAD|  Mean|    SD|  CI\_lower|  CI\_higher|    MPE|    BF|  Overlap| Rope      |
|---------------------------------------------------|-------:|-----:|-----:|-----:|----------:|-----------:|------:|-----:|--------:|:----------|
| df\$Concealing/df\$Tolerating |    0.07|  0.03|  0.07|  0.03|       0.03|        0.12|  99.56|  1.95|    19.43| Undecided |

Those indices include the ROPE decision criterion (see [Kruschke, 2018](http://journals.sagepub.com/doi/abs/10.1177/2515245918771304)) as well as the Maximum Probability of Effect (MPE, the probability that an effect is negative or positive and different from 0).

Posterior
=========

We can easily extract the posterior distribution to visualize the probability of possible effects.

``` r
posterior <- results$values$posterior

plot(density(posterior))
```

<img src="https://raw.githubusercontent.com/neuropsychology/psycho.R/master/docs/_posts/2018-06-11-bayesian_correlation_files/figure-markdown_github/unnamed-chunk-6-1.png" style="display: block; margin: auto;" />


Contribute
==========

Of course, these reporting standards should change, depending on new expert recommandations or official guidelines. **The goal of this package is to flexibly adaptive to new changes and good practices evolution**. Therefore, if you have any advices, opinions or such, we encourage you to either let us know by opening an [issue](https://github.com/neuropsychology/psycho.R/issues) or, even better, try to implement them yourself by [contributing](https://github.com/neuropsychology/psycho.R/blob/master/.github/CONTRIBUTING.md) to the code.


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
