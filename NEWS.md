# Current Dev



### Breaking changes
- Renamed `bayesian_cor` to `bayes_cor` for consistency
- Changed parameters name and role in `refdata`
- Changed output of analyze.stanreg
- effsize in analyze.stanreg default to TRUE
- Fixed bug in `dprime`
- `mellenbergh.test`, `crawford.test` and `crawford.test.freq` now return a psychobject
- `assess` has been refactored to become a wrapper for `crawford.test`
- `crawford.test` now computes the Bayesian version
- `crawford.test` has been renamed to `crawford.test.freq`
### New functions / parameters
- Added "bayesian" method for correlation matrix
- Added `reorder_matrix`
- Added `bayes_adj_R2` for loo-adjusted R2 in stanreg models
- Added `get_std_posteriors` for standardized coefs in Bayesian models
- First iteration on `bayesian_cor` for Bayesian correlation tables
- Added standardized coefs computation to analyze.stanreg
- Added `refdata` for reference grid creation
- Added `rope` for region of practical equivalence
- Added `interpret_r` for correlation coefficient interpretation
- Added `bayesian_cor.test`, start to work on implementation of bayesian method for correlation
- Added `find_matching_string` for fuzzy string matching
- Added  `analyze` for psych::fa objects
- Added `i_am_cheating` parameter to `correlation` to prevent p-hacking
- Added `percentile` and `percentile_to_z` functions
- Added `as.data.frame` method for density objects
- Added Crawford-Garthwaite (2007) Bayesian test for single-case analysis
- Added `rnorm_perfect` function
### Changes
- Improvements on analyze.stanreg
- Fixed `draws` parameter in get_predicted.stanreg
- logo



# [0.2.0](https://github.com/neuropsychology/psycho.R/releases/tag/0.2.0) (2018-05-01)

### Breaking changes
- Set default overlap parameter to TRUE in `analyze.stanreg` 
- Remove support of lmerMod objects (linear mixed models must be fitted with lmerTest)
- Refactored get_predicted.stanreg
- Changed organization of `values` in analyzed models
- Added `subset` parameter in `standardize`
- Changed order of parameters in `dprime`
### New functions / parameters
- Added `print` for `n_factors`
- Added `overlap` (experimental) parameter to analyze.stanreg as a different index of effect existence
- Added `overlap` function
- Added `power_analysis` function.
- Added `analyze.lm` for lm objects.
- Added `interpret_bf` for bayes factor interpretation
- Added `probs_to_odds`
- Added confidence intervals (CI) to analyze for lmerTest models
- Refactored `odds_to_probs`
- Added ìs.mixed.stanreg`
- Added `keep_iterations` in `get_predicted.stanreg` (and demonstration of how to plot them in vignettes)
- Added `emotion` dataset
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

