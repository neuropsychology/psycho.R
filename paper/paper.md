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

Psycho is an R package [@team2000r] that aims at providing tools for psychologists, neuropsychologists and neuroscientists, to transform statistical outputs into something readable that can be, almost directly, copied and pasted into a report. It also implements various functions useful in psychological science, such as correlation matrices, assessment plot creation or normalization. The package revolves around the psychobject. Main functions from the package return this type, and the `analyze()` function transforms other R objects into psychobjects. Four functions can then be applied on a psychobject: `summary()`, `print()`, `plot()` and `values()`. Contrary to many other packages which goal is to produce statistical analyzes, `psycho` aims at filling the gap between statistical R outputs and statistical report writing, with a focus on APA formatting guidelines, to enhance the standardization of results reporting. Complex outputs, such as those of Bayesian and frequentist mixed models, are automatically transformed into readable text, tables, and plots that illustrate the effects. Thus, the results can easily be incorporated into shareable reports and publications, promoting data exploration, saving time and preventing errors for better, reproducible, science. 
  
# References
