# Current Dev

### Breaking changes
- Changed organization of `values` in analyzed models
- Added `subset` parameter in `standardize`
- Changed order of parameters in `dprime`
### New functions / parameters
- Added0`emotion` dataset
- Added get_contrasts methods for lme4 objects
- Added prior info in `analyze.stanreg`
- Added (exposed) `interpret_d_posterior` for Bayesian size effect interpretation
- Added `find_combinations` function
- Added `find_best_model` function
- Added `is.standardized` function
- Added `get_contrasts.stanreg` and `get_predicted.stanreg` functions
- Added `crawford_dissociation.test` function for single-case tests
- Externalized the "mpe" function
- Added `affective` dataset
### Changes
- Added model selection chapter in Bayesian vignettes
- Refactor the `analyze.stanreg` code
- Improved `standardize` for vectors
- Added random effects summary in `analyze.stanreg`
- Improved `mellenbergh.test`
- Improved `analyze.stanreg`
- Added new vignette for "Bayesian analyses in psychology"
- Improved `crawford.test`

# [0.1.4](https://github.com/neuropsychology/psycho.R/releases/tag/0.1.4) (2018-03-23)


### Breaking changes
### New functions / parameters
- Added R2 for bayesian regressions
- Added `dprime` function for signal detection theory indices computation
- Added `crawford.test` and `mellenbergh.test` function for single-case tests
- Added get_predicted for stanreg models
### Changes


# [0.1.0](https://github.com/neuropsychology/psycho.R/releases/tag/0.1.0) (2018-02-05)

### Breaking changes
- `normalize` has been renamed to `standardize` #30
### New functions / parameters
- Added support to lme4's merMod objects
- Added `print` output to `correlation`
- Added glasso and cor_auto estimation for `correlation` #25 #24
- added `is.psychobject` function.
### Changes
- CRAN release
- Fixed #28
- Nicer print.psychobject output #32
- Replaced NAs by empty strings in `correlation` #23


# [0.0.8](https://github.com/neuropsychology/psycho.R/releases/tag/0.0.8) (2018-01-04)

### Breaking changes
### New functions / parameters
### Major changes
### Minor changes
- Added [`CONTRIBUTING.md`](https://github.com/neuropsychology/psycho.R/blob/master/CONTRIBUTING.md)
- Changed `format_digit`
- Added `except` parameter to `normalize`

# 0.0.7 (2018-01-10)

### Breaking changes
### New functions / parameters
### Major changes
### Minor changes
- Added [`CONTRIBUTING.md`](https://github.com/neuropsychology/psycho.R/blob/master/CONTRIBUTING.md)
- Changed `format_digit`
- Added `except` parameter to `normalize`


# 0.0.6 (2017-12-07)

### Breaking changes
### New functions / parameters
- `hdi`: Compute highest density intervals
- `format_string`: A tidyverse friendly version of `sprintf`
### Major changes
- Changed credible interval computation in analyze.stanreg
### Minor changes
- Use `styler`

# 0.0.4 (2017-11-15)

### Breaking changes
### New functions / parameters
- `correlation`: Plot is now supported by ggcorrplot instead of corrplot. The function behaves consistently (`plot(correlation(df)`)
### Major changes
- `correlation`: Fix p values adjustment
### Minor changes
- `analyze.stanreg`: Removed the mean and sd of the `print()`, added the MPE
- `analyze.stanreg`: Returns features of R2 for stan_lm

# 0.0.3 (2017-11-10)

### Breaking changes
### New functions / parameters
- `n_factors`: How many factors to retain for PCA or factor analysis?
### Major changes
### Minor changes


# 0.0.2 (2017-10-12)

### Breaking changes
### New functions / parameters
- analyze.merModLmerTest
- analyze.glmerMod
### Major changes
### Minor changes
- Improve testing


# [0.0.1](https://github.com/neuropsychology/psycho.R/releases/tag/0.0.1) (2017-10-06)

### Breaking changes
### New functions / parameters
### Major changes
- First CRAN release
### Minor changes

