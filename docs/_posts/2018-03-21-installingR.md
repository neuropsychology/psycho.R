---
layout: post
title:  "Installing R and R studio"
date:   2018-03-21
summary: Installing R and R studio
---

How to install R?

Installation
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

If you've never used `psycho`, enter this in the console and press enter:

``` r
install.packages("psycho")

# Or this for the dev version:
install.packages("devtools")
library(devtools)
devtools::install_github("https://github.com/neuropsychology/psycho.R")
```

Else, just put this at the beginning of every script:

``` r
library(psycho)
```
