---
title: 'The psycho Package: an Efficient and Publishing-Oriented Workflow for Psychological Science'
tags:
- R
- Psychology
- Interpratation
- Formatting
- Correlation
- Assessement
authors:
- name: Dominique Makowski
 orcid: 0000-0001-5375-9967
 affiliation: 1,2
affiliations:
- name: Memory and Cognition Lab', Institute of Psychology, University of Sorbonne Paris Cité, France
 index: 1
- name: INSERM U894, Center for Psychiatry and Neuroscience, Paris, France
 index: 2
date: 19 October 2017
bibliography: paper.bib
---

# Summary

Psycho is an R package [@team2000r] which main goal is to fill the gap between statistical analyses and publication-ready text, involving information selection, results transformation and text formatting. These steps, beyond being the most time-consuming part of statistical analysis in psychological science, are the source of many human errors and can lead to a badly formatted results section, veiling the general quality of the paper. The `psycho` package, built for psychologists, neuropsychologists and neuroscientists, also implements commonly used routines such as correlation matrices, assessment plot creation or normalisation. The package revolves around the `psychobject`, returned by most of the functions. Additionally, the `analyse()` function transforms other R objects into psychobjects. Four methods can then be applied on them: `summary()`, `print()`, `plot()` and `values()`. Contrary to many other packages which goal is to produce statistical analyses, `psycho` bridges statistical R outputs with statistical report writing, with a focus on APA formatting guidelines, to enhance the standardisation of results reporting. Complex outputs, such as those of Bayesian and frequentist mixed models, are automatically transformed into readable text, tables, and plots that illustrate the effects. With that package, results can be easily  incorporated into sharable reports and publications, promoting data exploration, saving time and preventing errors for better, reproducible, science.
  
# References
