---
title: 'mbir: Magnitude-Based Inferences'
authors:
- affiliation: 1
  name: Kyle D Peterson
  orcid: 0000-0003-0128-5282
- affiliation: 2
  name: Aaron R Caldwell
  orcid: 0000-0002-4541-6283
date: "04 May 2018"
output:
  pdf_document: default
bibliography: paper.bib
affiliations:
- index: 1
  name: Sports Science, University of Iowa
- index: 2
  name: Exercise Science Research Center, University of Arkansas
---

# Summary

`mbir` is an R package [@mbir] to provide practitioners and researchers a wholesale approach for deriving magnitude-based inferences from raw data. Magnitude-based inferences, popularized by Hopkins et al [@hopkins08], was originally constructed for sport practitioners to derive qualitative interpretations of effect statistics via Microsoft Excel\circledR\ spreadsheets [@sportsci]. `mbir` is now the first open-source solution to such methodology in both R [@rbase] and jamovi [@jamovi].

For a concise description, magnitude-based inferences offer a form of statistical inference in which the parameter is partitioned into three regions corresponding to various qualitative aspects. A quasi-probability (MBI%) is also assigned to each of these regions, calculated from the assumed distribution of the parameter estimate after being adjusted by the calculated effect size. `mbir` functions provide qualitative labels to comprise content-rich inferential statements; taking into consideration the level of confidence (*possibly*, *likley*, etc), level of magnitude (*trivial*, *small*, *moderate*, etc), as well as the direction of effect (*negative*, *positive*). 

The primary objective of `mbir` is to programmatically detect appropriate statistical tests to run in order to drastically curtail the statistical degrees of freedom in an automated fashion. Users simply supply data vectors and allow `mbir` functions to make statistically-sound decisions, perform respective calculations, and highlight the most suitable interpretations. Figure 1 exemplifies the logic behind a standardized mean difference (*t*-test) function. In this case, establishing whether the distributions deviate from a comparably normal shape and whether homogeneity of variance between vectors is present, will dictate which statistical test to apply to arrive at proper effect statistic. Although such logic are default settings, users also have the freedom to code their own statistical decisions independently, if desired.

-![figure 1](twosample.jpg)

Outside of standard *t*-tests and correlations, `mbir` offers a collection of strategies for calculating *a priori* sample size estimations, nonparametric confidence intervals via bootstrap resampling, and a couple ways to analyze individual longitudinal trends via linear regression.

# References
