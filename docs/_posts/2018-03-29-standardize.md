---
layout: post
title: "Standardize (Z-score) a dataframe"
author: Dominique Makowski
author_web: https://dominiquemakowski.github.io/
date: 2018-03-29
summary: Standardize (Z-score) a dataframe
---

Standardize / Normalize / Z-score / Scale
-----------------------------------------

The `standardize()` function allows you to easily scale and center all numeric variables of a dataframe. It is similar to the base function `scale()`, but presents some advantages: it is tidyverse-friendly, data-type friendly (*i.e.*, does not transform it into a matrix) and can handle dataframes with categorical data.

``` r
library(psycho)
library(tidyverse)

z_iris <- iris %>% 
  psycho::standardize() 

summary(z_iris)
```

           Species    Sepal.Length       Sepal.Width       Petal.Length    
     setosa    :50   Min.   :-1.86378   Min.   :-2.4258   Min.   :-1.5623  
     versicolor:50   1st Qu.:-0.89767   1st Qu.:-0.5904   1st Qu.:-1.2225  
     virginica :50   Median :-0.05233   Median :-0.1315   Median : 0.3354  
                     Mean   : 0.00000   Mean   : 0.0000   Mean   : 0.0000  
                     3rd Qu.: 0.67225   3rd Qu.: 0.5567   3rd Qu.: 0.7602  
                     Max.   : 2.48370   Max.   : 3.0805   Max.   : 1.7799  
      Petal.Width     
     Min.   :-1.4422  
     1st Qu.:-1.1799  
     Median : 0.1321  
     Mean   : 0.0000  
     3rd Qu.: 0.7880  
     Max.   : 1.7064  

But beware, standardization **does not** change (and "normalize") the distribution!

``` r
z_iris %>% 
  dplyr::select(-Species) %>% 
  gather(Variable, Value) %>% 
  ggplot(aes(x=Value, fill=Variable)) +
      geom_density(alpha=0.5) +
      geom_vline(aes(xintercept=0)) +
      theme_bw() +
      scale_fill_brewer(palette="Spectral")
```

<img src="https://raw.githubusercontent.com/neuropsychology/psycho.R/master/docs/_posts/2018-03-29-standardize_files/figure-markdown_github/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />


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
