---
layout: post
title: "How to Cite Packages"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-08-31
summary: Citing the packages, modules and softwares you used for your analysis is important.
---

-   [What should I cite?](#what-should-i-cite)
-   [How should I cite it?](#how-should-i-cite-it)
-   [Previous blogposts](#previous-blogposts)

**Citing the packages, modules and softwares you used for your analysis is important**, both from a **reproducibility perspective** (statistical routines are often implemented in different ways by different packages, which could explain slight discrepancies in the results. Saying "*I did this using this function from that package version 1.2.3*" is a way of protecting yourself by being clear about what you have found doing what you have done) but also for **acknowledging** the work and time that people spent creating tools for others (sometimes at the expense of their own research).

-   **That's great, but how to *actually* cite them?**
-   **I used about 100 packages, should I cite them *all*?**

What should I cite?
===================

Ideally, you should indeed cite all the packages that you used. However, it's not very diegetic. Therefore, I would recommand the following:

1.  Cite the main / important packages in the manuscript

This should be done for the packages that were central to your specific analysis (*i.e.,* that got you the results that you reported) rather than data manipulation tools (even though these are as much important).

For example:

> Statistics were done using R 3.5.0 (R Core Team, 2018), the *rstanarm* (*v2.13.1*; Gabry & Goodrich, 2016) and the *psycho* (*v0.3.4*; Makowski, 2018) packages. The full reproducible code is available in **Supplementary Materials**.

2.  Present everything in Supplementary Materials

Then, in Supplementary Materials, you show the packages and functions you used. Moreover, in R, you can include (usually at the end) every used package and their version using the `sessionInfo()` function.

How should I cite it?
=====================

Finding the right citation information is sometimes complicated. In R, this process is made quite easy, you simply run `citation("packagename")`. For instance, `citation("dplyr")`:

    To cite ‘dplyr’ in publications use:

      Hadley Wickham, Romain François, Lionel Henry and Kirill Müller (2018). dplyr: A Grammar of Data Manipulation. R package version
      0.7.6. https://CRAN.R-project.org/package=dplyr

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {dplyr: A Grammar of Data Manipulation},
        author = {Hadley Wickham and Romain François and Lionel Henry and Kirill Müller},
        year = {2018},
        note = {R package version 0.7.6},
        url = {https://CRAN.R-project.org/package=dplyr},
      }

For other languages, such as Python or Julia, it might be a little trickier, but a **quick search on google (*or github*)** should provide you with all the necessary information (version, authors, date). **It's better to have a slightly incomplete citation than no citation at all.**

Previous blogposts
==================

-   [The end of errors in ANOVA reporting](https://neuropsychology.github.io/psycho.R/2018/07/20/analyze_anova.html)
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
