<p align="center"><a href=https://github.com/neuropsychology/psycho.R/><img src="https://github.com/neuropsychology/psycho.R/blob/master/vignettes/images/logo.PNG" width="400" align="center" alt="psycho logo r package"></a></p>


*<h4 align="center">Efficient and Publishing-oriented Workflow for Psychological Science</h2>*


# psycho
[![Build Status](https://travis-ci.org/neuropsychology/psycho.R.svg?branch=master)](https://travis-ci.org/neuropsychology/psycho.R)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Dependency Status](https://dependencyci.com/github/neuropsychology/psycho.R/badge)](https://dependencyci.com/github/neuropsychology/psycho.R)
[![Build status](https://ci.appveyor.com/api/projects/status/08mg1fshh5iqx53b?svg=true)](https://ci.appveyor.com/project/DominiqueMakowski/psycho-r)
[![codecov](https://codecov.io/gh/neuropsychology/psycho.R/branch/master/graph/badge.svg)](https://codecov.io/gh/neuropsychology/psycho.R)

|Name|NeuroKit|
|----------------|---|
|Documentation|[![](https://img.shields.io/badge/docs-Building-orange.svg?colorB=FF5722)](https://github.com/neuropsychology/psycho.R/blob/master/vignettes/overview.R)|
|Questions|[![](https://img.shields.io/badge/issue-create-purple.svg?colorB=FF9800)](https://github.com/neuropsychology/psycho.R/issues)|
|Authors|[![](https://img.shields.io/badge/CV-D._Makowski-purple.svg?colorB=9C27B0)](https://dominiquemakowski.github.io/)|

---


## Goal

The main goal of the `psycho` package is to provide tools for psychologists, neuropsychologists and neuroscientists, to transform statistical outputs into something readable that can be, almost directly, copied and pasted into a report. It also implements various functions, from very useful ones (`correlation()`, `normalize()`) to miscellaenous ones (`find_season()`).


## Contribute

Want to get involved in the developpment of an open-source software and improve psychological science? **Join us!**

- You need some help? You found a bug? You would like to request a new feature? 
  Just open an [issue](https://github.com/neuropsychology/psycho.R/issues) :relaxed:
- Want to add a feature? Correct a bug? You're more than welcome to contribute!
- **Call for help for implementation of `analyze` method for `t.test`, `cor.test`, `lm` and `lmer`**.
  
  
## Installation

Open R and run the following:

```R
install.packages("devtools")
library("devtools")
install_github("neuropsychology/psycho.R")
library("psycho")
```

## General Workflow

The package mainly revolves around the `psychobject`. Main functions from the package return this type, and the `analyze()` function transforms other R objects (*for now, only `stan_lmer` type*) into psychobjects. Then, 4 functions can be applied on a psychobject: `summary()`, `print()`, `plot()` and `values()`.



![](https://github.com/neuropsychology/psycho.R/blob/master/vignettes/images/workflow.PNG)
