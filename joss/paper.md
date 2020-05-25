---
title: 'Scoring the implicit: The implicitMeasures package'
authors:
- affiliation: '1'
  name: Ottavia M. Epifania
  orcid: 0000-0001-8552-568X
- affiliation: '1'
  name: Pasquale Anselmi
  orcid:  0000-0003-2982-7178
- affiliation: '1'
  name: Egidio Robusto
date: "24 aprile, 2020"
output: 
  # pdf_document: default
  html_document:
       keep_md: true
bibliography: paper.bib
tags:
- R
- Social Pscyhology
- Implicit Social Cognition
- Implicit Association Test
- Single Category Implicit Association Test
- Replicability
affiliations:
- index: 1
  name: Department of Philosophy, Sociology, Education and Applied Psychology, University
    of Padova (IT)
---

# Statement of need

Within the past decades, social sciences have shown a growing interest in the implicit investigation of attitudes. The Implicit Association Test [IAT; @Greenwald:1998] and the Single Category IAT [SC-IAT; @karpinski2006] are the most common measures used for this
aim. Both tests result in a differential score (the *D-score*) expressing respondents’ bias in categorizing different stimuli in two contrasting conditions. While the scoring of the SC-IAT is based only on one algortihm [@karpinski2006], six different algorithms are availbale for computing the IAT *D-score* [@Greenwald2003]. The core procedure for the computation of the IAT *D-score* is the same for all the alogirithms, which differentiate themsleves according for the treatment of extreme fast responses and the replacement of error responses. 

Despite that many R packages exist for computing IAT *D-score* algorithms, no packages exist for scoring the SC-IAT. Additionally, majority of existing R packages created for the computation of IAT *D-score* algorithms do not provide all the available algorithms. The packages allowing for the computation of multiple *D-score* algorithms either do not offer the chance to compare their results, or do not disambiguate the specific algorithm they are computing, raising reproducibility issue [@ellithorpe2015]. 

Recently, a Shiny Web Application [@shiny] has been developed for computing the IAT *D-score*, called *DscoreApp* [@dscoreapp]. This app proied an inutitive and easy to use user interface. By providing a detailed explanation of the *D-score* algortihms that can be computed, *DscoreApp* addresses the majority of the above mentioned replicabilit issues. Moreover, the graphical representation of the results can give an immediate glimpse of the results. However, *DscoreApp* presents some shortcomings as well. Firstly, since it is a shiny app, it is associated with the most oustanding issue of the shiny aps in general, namely, the replicabilit of the results. Specifically, since it is not possible to access the code used for the computation, it is not possible to replicate the results and even check whether the code presents some mistakes. However, @dscoreapp used a GitHub repository to let public access the code used for the compusation. Despite the graphical representations of the results provided by *DscoreApp* are really useful for getting a first idea on the IAT results and they are all downloadble in a .pdf format,  they cannot be changed by the users. Moreover, *DscoreApp* computes the *D-score* for only the IAT. 

`implicitMeasures` package is an `R` package aimed at overcoming both the shortcomings of the existing `R` packages for the computaion of the IAT *D-score* and those of the shiny app *DscoreApp*.
`implicitMeasures` is an `R` package that provides an easy and open source way to score both the IAT and the SC-IAT, and to easily compare different IAT *D-score* algorithms. 

# Overvie of `implicitMeasures` package

# References
