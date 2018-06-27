---
layout: post
title: "Beautiful and Powerful Correlation Tables in R"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-05-20
summary: Beautiful and Powerful Correlation Tables in R.
---

-   [Another *correlation* function?!](#another-correlation-function)
-   [A table](#a-table)
-   [A Plot](#a-plot)
-   [A print](#a-print)
-   [Options](#options)
-   [Fun with *p*-hacking](#fun-with-p-hacking)
-   [Credits](#credits)

Another *correlation* function?!
================================

Yes, the `correlation` function from the [`psycho`](https://github.com/neuropsychology/psycho.R) package.

``` r
devtools::install_github("neuropsychology/psycho.R")  # Install the newest version

library(psycho)
library(tidyverse)

cor <- psycho::affective %>% 
  correlation()
```

This function automatically select numeric variables and run a correlation analysis. It returns a `psychobject`.

A table
=======

We can then extract a **formatted table** that can be saved and pasted into reports and manuscripts by using the `summary` function.

``` r
summary(cor)
# write.csv(summary(cor), "myformattedcortable.csv")
```

|                    | Age   | Life\_Satisfaction | Concealing | Adjusting  |
|--------------------|:------|:-------------------|:-----------|:-----------|
| Age                |       |                    |            |            |
| Life\_Satisfaction | 0.03  |                    |            |            |
| Concealing         | -0.05 | -0.06              |            |            |
| Adjusting          | 0.03  | 0.36\*\*\*         | 0.22\*\*\* |            |
| Tolerating         | 0.03  | 0.15\*\*\*         | 0.07       | 0.29\*\*\* |

A Plot
======

It integrates a **plot** done with [`ggcorplot`](https://github.com/kassambara/ggcorrplot).

``` r
plot(cor)
```

<img src="https://raw.githubusercontent.com/neuropsychology/psycho.R/master/docs/_posts/2018-05-20-correlation_files/figure-markdown_github/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

A print
=======

It also includes a **pairwise correlation printing** method.

``` r
print(cor)
```

    Pearson Full correlation (p value correction: holm):

       - Age / Life_Satisfaction:   Results of the Pearson correlation showed a non significant and weak negative association between Age and Life_Satisfaction (r(1249) = 0.030, p > .1).
       - Age / Concealing:   Results of the Pearson correlation showed a non significant and weak positive association between Age and Concealing (r(1249) = -0.050, p > .1).
       - Life_Satisfaction / Concealing:   Results of the Pearson correlation showed a non significant and weak positive association between Life_Satisfaction and Concealing (r(1249) = -0.063, p > .1).
       - Age / Adjusting:   Results of the Pearson correlation showed a non significant and weak negative association between Age and Adjusting (r(1249) = 0.027, p > .1).
       - Life_Satisfaction / Adjusting:   Results of the Pearson correlation showed a significant and moderate negative association between Life_Satisfaction and Adjusting (r(1249) = 0.36, p < .001***).
       - Concealing / Adjusting:   Results of the Pearson correlation showed a significant and weak negative association between Concealing and Adjusting (r(1249) = 0.22, p < .001***).
       - Age / Tolerating:   Results of the Pearson correlation showed a non significant and weak negative association between Age and Tolerating (r(1249) = 0.031, p > .1).
       - Life_Satisfaction / Tolerating:   Results of the Pearson correlation showed a significant and weak negative association between Life_Satisfaction and Tolerating (r(1249) = 0.15, p < .001***).
       - Concealing / Tolerating:   Results of the Pearson correlation showed a non significant and weak negative association between Concealing and Tolerating (r(1249) = 0.074, p = 0.05°).
       - Adjusting / Tolerating:   Results of the Pearson correlation showed a significant and weak negative association between Adjusting and Tolerating (r(1249) = 0.29, p < .001***).

Options
=======

You can also cutomize the **type** (*pearson, spearman or kendall*), the **p value correction method** (*holm (default), bonferroni, fdr, none...*) and run **partial, semi-partial** or **glasso** correlations.

``` r
psycho::affective %>% 
  correlation(method = "pearson", adjust="bonferroni", type="partial") %>% 
  summary()
```

|                    | Age   | Life\_Satisfaction | Concealing | Adjusting  |
|--------------------|:------|:-------------------|:-----------|:-----------|
| Age                |       |                    |            |            |
| Life\_Satisfaction | 0.01  |                    |            |            |
| Concealing         | -0.06 | -0.16\*\*\*        |            |            |
| Adjusting          | 0.02  | 0.36\*\*\*         | 0.25\*\*\* |            |
| Tolerating         | 0.02  | 0.06               | 0.02       | 0.24\*\*\* |

Fun with *p*-hacking
====================

In order to **prevent people for running many uncorrected correlation tests** (promoting *p*-hacking and result-fishing), we included the `i_am_cheating` parameter. If FALSE (default), the function will help you finding interesting results!

``` r
df_with_11_vars <- data.frame(replicate(11, rnorm(1000)))
cor <- correlation(df_with_11_vars, adjust="none") 
```

    ## Warning in correlation(df_with_11_vars, adjust = "none"): We've detected that you are running a lot (> 10) of correlation tests without adjusting the p values. To help you in your p-fishing, we've added some interesting variables: You never know, you might find something significant!
    ## To deactivate this, change the 'i_am_cheating' argument to TRUE.

``` r
summary(cor)
```

|                             | X1         | X2          | X3          | X4          | X5          | X6          | X7          | X8          | X9          | X10         | X11         |
|-----------------------------|:-----------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|:------------|
| X1                          |            |             |             |             |             |             |             |             |             |             |             |
| X2                          | -0.04      |             |             |             |             |             |             |             |             |             |             |
| X3                          | -0.04      | -0.02       |             |             |             |             |             |             |             |             |             |
| X4                          | 0.02       | 0.05        | -0.02       |             |             |             |             |             |             |             |             |
| X5                          | -0.01      | -0.02       | 0.05        | -0.03       |             |             |             |             |             |             |             |
| X6                          | -0.03      | 0.03        | 0.08\*      | 0.02        | 0.02        |             |             |             |             |             |             |
| X7                          | 0.03       | -0.01       | -0.02       | -0.04       | -0.03       | -0.04       |             |             |             |             |             |
| X8                          | 0.01       | -0.07\*     | 0.04        | 0.02        | -0.01       | -0.01       | 0.00        |             |             |             |             |
| X9                          | -0.02      | 0.03        | -0.03       | -0.02       | 0.00        | -0.04       | 0.03        | -0.02       |             |             |             |
| X10                         | -0.03      | 0.00        | 0.00        | 0.01        | 0.01        | -0.01       | 0.01        | -0.02       | 0.02        |             |             |
| X11                         | 0.01       | 0.01        | -0.03       | -0.05       | 0.00        | 0.05        | 0.01        | 0.00        | -0.01       | 0.07\*      |             |
| Local\_Air\_Density         | 0.26\*\*\* | -0.02       | -0.44\*\*\* | -0.15\*\*\* | -0.25\*\*\* | -0.50\*\*\* | 0.57\*\*\*  | -0.11\*\*\* | 0.47\*\*\*  | 0.06        | 0.01        |
| Reincarnation\_Cycle        | -0.03      | -0.02       | 0.02        | 0.04        | 0.01        | 0.00        | 0.05        | -0.04       | -0.05       | -0.01       | 0.03        |
| Communism\_Level            | 0.58\*\*\* | -0.44\*\*\* | 0.04        | 0.06        | -0.10\*\*   | -0.18\*\*\* | 0.10\*\*    | 0.46\*\*\*  | -0.50\*\*\* | -0.21\*\*\* | -0.14\*\*\* |
| Alien\_Mothership\_Distance | 0.00       | -0.03       | 0.01        | 0.00        | -0.01       | -0.03       | -0.04       | 0.01        | 0.01        | -0.02       | 0.00        |
| Schopenhauers\_Optimism     | 0.11\*\*\* | 0.31\*\*\*  | -0.25\*\*\* | 0.64\*\*\*  | -0.29\*\*\* | -0.15\*\*\* | -0.35\*\*\* | -0.09\*\*   | 0.08\*      | -0.22\*\*\* | -0.47\*\*\* |
| Hulks\_Power                | 0.03       | 0.00        | 0.02        | 0.03        | -0.02       | -0.01       | -0.05       | -0.01       | 0.00        | 0.01        | 0.03        |

**As we can see, Schopenhauer's Optimism is strongly related to many variables!!!**

Credits
=======

This package was useful? You can cite [`psycho`](https://github.com/neuropsychology/psycho.R) as follows:

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
