<p align="center"><a href=https://neuropsychology.github.io/psycho.R/><img src="https://github.com/neuropsychology/psycho.R/blob/master/vignettes/images/logo.PNG" width="400" align="center" alt="psycho logo r package"></a></p>


*<h4 align="center">Efficient and Publishing-Oriented Workflow for Psychological Science</h2>*


# psycho
[![Build Status](https://travis-ci.org/neuropsychology/psycho.R.svg?branch=master)](https://travis-ci.org/neuropsychology/psycho.R)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CRAN downloads total](http://cranlogs.r-pkg.org/badges/grand-total/psycho)](https://CRAN.R-project.org/package=psycho)
[![Build status](https://ci.appveyor.com/api/projects/status/08mg1fshh5iqx53b?svg=true)](https://ci.appveyor.com/project/DominiqueMakowski/psycho-r)
[![codecov](https://codecov.io/gh/neuropsychology/psycho.R/branch/master/graph/badge.svg)](https://codecov.io/gh/neuropsychology/psycho.R)
[![Dependency Status](https://dependencyci.com/github/neuropsychology/psycho.R/badge)](https://dependencyci.com/github/neuropsychology/psycho.R)
[![CRAN downloads month](https://cranlogs.r-pkg.org/badges/psycho)](https://CRAN.R-project.org/package=psycho)




|Name|psycho|
|----------------|---|
|Stable|[![CRAN](https://www.r-pkg.org/badges/version/psycho)](https://CRAN.R-project.org/package=psycho)|
|Documentation|[![Rdoc](https://www.rdocumentation.org/badges/version/psycho)](https://www.rdocumentation.org/packages/psycho)|
|Blog|[![](https://img.shields.io/badge/blog-psycho-orange.svg?colorB=E91E63)](https://neuropsychology.github.io/psycho.R)|
|Examples|[![](https://img.shields.io/badge/vignettes-0.1.4-orange.svg?colorB=FF5722)](https://CRAN.R-project.org/package=psycho/vignettes/overview.html)|
|Questions|[![](https://img.shields.io/badge/issue-create-purple.svg?colorB=FF9800)](https://github.com/neuropsychology/psycho.R/issues)|
|Authors|[![](https://img.shields.io/badge/CV-D._Makowski-purple.svg?colorB=9C27B0)](https://dominiquemakowski.github.io/)|
|Reference|[![DOI](http://joss.theoj.org/papers/10.21105/joss.00470/status.svg)](https://doi.org/10.21105/joss.00470)|


---


## Goal

The main goal of the `psycho` package is to provide tools for psychologists, neuropsychologists and neuroscientists, to transform statistical outputs into something readable that can be, almost directly, copied and pasted into a report. It also implements various functions, from very useful ones (`correlation()`, `standardize()`) to miscellaenous ones (`find_season()`).


## Contribute

Want to get involved in the developpment of an open-source software and improve psychological science? **Join us!**

- You need some help? You found a bug? You would like to request a new feature? 
  Just open an [issue](https://github.com/neuropsychology/psycho.R/issues) :relaxed:
- Want to add a feature? Correct a bug? You're more than welcome to contribute!
- Looking for help to implement the `analyze` method for `t.test`, `cor.test` and `anova`.
  
  
## Features

The `psycho` package can already do the following:

- [x] Standardize your data
- [x] Enlight you on how many factors to retain for a PCA
- [x] Give you some clinically relevant info on a participant's score
- [x] Implements methods for single-case analyses
- [x] Compute complex correlation matrices
- [x] Compute signal detection theory indices (d', beta, ...)
- [x] Help you in the interpretation of various models (mixed, Bayesian, ...)



## General Workflow

The package revolves around the `psychobject`. Main functions from the package return this type, and the `analyze()` function transforms other R objects into psychobjects. Four functions can then be applied on a psychobject: `summary()`, `print()`, `plot()` and `values()`.



![](https://github.com/neuropsychology/psycho.R/blob/master/vignettes/images/workflow.PNG)


## Installation

- To get the stable version from CRAN, run the following commands in your R console:

```R
install.packages("psycho")
library("psycho")
```

- To get the latest development version, run the following:
```R
install.packages("devtools")
library("devtools")
install_github("neuropsychology/psycho.R")
library("psycho")
```

## Credits

You can cite the package as following:
- Makowski, (2018). The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science. Journal of Open Source Software, 3(22), 470. https://doi.org/10.21105/joss.00470


Please remember that `psycho` is a high-level package that heavily relies on many other packages, such as [tidyverse](https://www.tidyverse.org/), [psych](http://personality-project.org/r/overview.pdf), [qgraph](http://sachaepskamp.com/qgraph), [rstanarm](https://github.com/stan-dev/rstanarm), [lme4](https://CRAN.R-project.org/package=lme4) and others (See [Description](https://github.com/neuropsychology/psycho.R/blob/master/DESCRIPTION) for the full list of dependencies). Please cite their authors ;)
