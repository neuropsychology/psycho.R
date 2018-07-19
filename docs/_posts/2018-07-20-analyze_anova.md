---
layout: post
title: "The end of errors in ANOVA reporting"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-07-20
summary: The end of errors in ANOVA reporting.
---

-   [Fit an anova](#fit-an-anova)
-   [APA formatted output](#apa-formatted-output)
-   [Correlations, t-tests, regressions...](#correlations-t-tests-regressions...)
-   [Evolution](#evolution)
-   [Credits](#credits)
-   [On similar topics](#on-similar-topics)

**Psychology is still ([unfortunately](https://neuropsychology.github.io/psycho.R/2018/05/01/repeated_measure_anovas.html)) massively using analysis of variance (ANOVA)**. Despite its relative simplicity, I am very often confronted to **errors in its reporting**, for instance in student's theses or manuscripts. Beyond the incomplete, uncomprehensible or just wrong reporting, one can find a tremendous amount of genuine errors (that could influence the results and their intepretation), even in published papers! (See the excellent [statcheck](http://statcheck.io/) to quickly check the stats of a paper). This error proneness can be at least partially explained by the fact that copy/pasting the (appropriate) values of any statistical software and formatting them textually is a very annoying process.

**How to end it?**

We believe that this could be solved (at least, partially) by the **default implementation of current best practices of statistical reporting**. A tool that automatically transforms a statistical result into a copy/pastable text. Of course, this automation cannot be suitable for each and every advanced usage, but would probably be satisfying for a substantial proportion of use cases. **Implementing this unified, end-user oriented pipeline is the goal of the [psycho](https://github.com/neuropsychology/psycho.R) package.**

Fit an anova
============

Let's start by doing a traditional ANOVA with *adjusting* (the ability to flexibly regulate one's emotions) as dependent variable, and *sex* and *salary* as categorical predictors.

``` r
# devtools::install_github("neuropsychology/psycho.R")  # Install the latest psycho version
library(psycho)

df <- psycho::affective  # load a dataset available in the psycho package

aov_results <- aov(Adjusting ~ Sex * Salary, data=df)  # Fit the ANOVA
summary(aov_results)  # Inspect the results
```

                 Df Sum Sq Mean Sq F value   Pr(>F)    
    Sex           1   35.9   35.94  18.162 2.25e-05 ***
    Salary        2    9.4    4.70   2.376   0.0936 .  
    Sex:Salary    2    3.0    1.51   0.761   0.4674    
    Residuals   859 1699.9    1.98                     
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    386 observations deleted due to missingness

APA formatted output
====================

The `psycho` package include a simple function, `analyze()` that can be applied to an ANOVA object to format its content.

``` r
analyze(aov_results)
```

       - The effect of Sex is significant (F(1, 859) = 18.16, p < .001) and can be considered as small (Partial Omega-squared = 0.019).
       - The effect of Salary is not significant (F(2, 859) = 2.38, p = 0.09°) and can be considered as very small (Partial Omega-squared = 0.0032).
       - The interaction between Sex and Salary is not significant (F(2, 859) = 0.76, p > .1) and can be considered as very small (Partial Omega-squared = 0).

It formats the results, computes the partial omega-squared as an index of effect size (better than the eta2, see [Levine et al. 2002](https://academic.oup.com/hcr/article-abstract/28/4/612/4331349), [Pierce et al. 2004](http://journals.sagepub.com/doi/abs/10.1177/0013164404264848)) as well as its [interpretation](http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/effectSize) and presents the results in a APA-compatible way.

Correlations, t-tests, regressions...
=====================================

Note that the `analyze()` method also exists for other statistical procudures, such as [correlations](https://neuropsychology.github.io/psycho.R/2018/06/28/analyze_correlation.html), [t-tests](https://neuropsychology.github.io/psycho.R/2018/06/19/analyze_ttest.html) and [regressions](https://neuropsychology.github.io/psycho.R/2018/05/01/repeated_measure_anovas.html).

Evolution
=========

Of course, these reporting standards should change, depending on new expert recommandations or official guidelines. **The goal of this package is to flexibly adaptive to new changes and good practices evolution**. Therefore, if you have any advices, opinions or such, we encourage you to either let us know by opening an [issue](https://github.com/neuropsychology/psycho.R/issues), or even better, try to implement them yourself by [contributing](https://github.com/neuropsychology/psycho.R/blob/master/.github/CONTRIBUTING.md) to the code.

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: An Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>

On similar topics
=================

-   [Variable vs. Participant-wise Standardization](https://neuropsychology.github.io/psycho.R/2018/07/14/standardize_grouped_df.html)
-   [Formatted Correlation with Effect Size](https://neuropsychology.github.io/psycho.R/2018/06/28/analyze_correlation.html)
-   [Extracting a Reference Grid of your Data for Machine Learning Models Visualization](https://neuropsychology.github.io/psycho.R/2018/06/25/refdata.html)
-   [Copy/paste t-tests Directly to Manuscripts](https://neuropsychology.github.io/psycho.R/2018/06/19/analyze_ttest.html)
-   [Easy APA Formatted Bayesian Correlation](https://neuropsychology.github.io/psycho.R/2018/06/11/bayesian_correlation.html)
-   [Fancy Plot (with Posterior Samples) for Bayesian Regressions](https://neuropsychology.github.io/psycho.R/2018/06/03/plot_bayesian_model.html)
-   [How Many Factors to Retain in Factor Analysis](https://neuropsychology.github.io/psycho.R/2018/05/24/n_factors.html)
-   [Beautiful and Powerful Correlation Tables](https://neuropsychology.github.io/psycho.R/2018/05/20/correlation.html)
-   [Format and Interpret Linear Mixed Models](https://neuropsychology.github.io/psycho.R/2018/05/10/interpret_mixed_models.html)
-   [How to do Repeated Measures ANOVAs](https://neuropsychology.github.io/psycho.R/2018/05/01/repeated_measure_anovas.html)
-   [Standardize (Z-score) a dataframe](https://neuropsychology.github.io/psycho.R/2018/03/29/standardize.html)
-   [Compute Signal Detection Theory Indices](https://neuropsychology.github.io/psycho.R/2018/03/29/SDT.html)
-   [Installing R, R Studio and psycho](https://neuropsychology.github.io/psycho.R/2018/03/21/installingR.html)
