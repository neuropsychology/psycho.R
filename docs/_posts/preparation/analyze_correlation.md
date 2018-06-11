-   [Do a correlation](#do-a-correlation)
-   [APA formatted output](#apa-formatted-output)
-   [Dataframe of Values](#dataframe-of-values)
-   [Bayesian Correlation](#bayesian-correlation)
-   [Contribute](#contribute)
-   [Credits](#credits)

One of the most time-consuming part of data analysis in psychology is the copy-pasting of specific values of some R output to a manuscript or a report. This task is frustrating, prone to errors, and increase de variability of statistical reporting. At the sime time, standardizing practices of what and how to report is crucial for reproducibility and clarity. **The [psycho](https://github.com/neuropsychology/psycho.R) package was designed specifically to do this job**, at first for complex [Bayesian mixed models](https://cran.r-project.org/web/packages/psycho/vignettes/bayesian.html), but is now also compatible with basic methods, such as **correlation**.

Do a correlation
================

``` r
df <- iris
cor_results <- cor.test(df$Sepal.Length, df$Petal.Length)
```

APA formatted output
====================

``` r
# devtools::install_github("neuropsychology/psycho.R")  # Install the latest psycho version

# Load packages
library(psycho)
psycho::analyze(cor_results)
```

    The Pearson's product-moment correlation between df$Sepal.Length and df$Petal.Length is significantly large and positive (r(148) = 0.87, 95% CI [0.83, 0.91], p < .001)

The formatted output includes **direction**, **effect size** (interpreted by default with **[Cohen's (1988)](https://github.com/neuropsychology/psycho.R/blob/master/R/interpret_r.R#L142)** rules) and **confidence intervals**. Now you can just copy and paste this line into your report and focus on what's important.

Dataframe of Values
===================

It is also possible to have all the values stored in a dataframe by running a **summary** on the analyzed object.

``` r
results <- analyze(cor_results)
summary(results)
```

|  effect|  statistic|   df|    p|  CI\_lower|  CI\_higher|
|-------:|----------:|----:|----:|----------:|-----------:|
|    0.87|      21.65|  148|    0|       0.83|        0.91|

Bayesian Correlation
====================

Nevertheless, we recommand doing a **Bayesian correlation**, which is even [easier and quicker to do](blogpostlink)!

Contribute
==========

Of course, these reporting standards should change, depending on new expert recommandations or official guidelines. **The goal of this package is to flexibly adaptive to new changes and good practices evolution**. Therefore, if you have any advices, opinions or such, we encourage you to either let us know by opening an [issue](https://github.com/neuropsychology/psycho.R/issues), or even better, try to implement them yourself by [contributing](https://github.com/neuropsychology/psycho.R/blob/master/.github/CONTRIBUTING.md) to the code.

Credits
=======

This package helped you? Don't forget to cite the various packages you used :)

You can cite `psycho` as follows:

-   Makowski, (2018). *The psycho Package: An Efficient and Publishing-Oriented Workflow for Psychological Science*. Journal of Open Source Software, 3(22), 470. <https://doi.org/10.21105/joss.00470>
