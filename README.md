
<!-- README.md is generated from README.Rmd. Please edit that file -->

# implicitMeasures

R package for computing different Implicit Measures scores
<!-- badges: start --> [![Travis build
status](https://travis-ci.org/OttaviaE/implicitMeasures.svg?branch=master)](https://travis-ci.org/OttaviaE/implicitMeasures)
<!-- badges: end -->

## Overview

The goal of implicitMeasures is to provide functions for cleaning data
from different implicit measures, computing their scores, and plotting
the results. So far, functions for the computing the scores for the
Implicit Association Test (IAT) and the Single Category-IAT (SC-IAT) are
available:

  - `clean_iat()` cleans and prepares the data for computing the IAT
    *D-score*.
  - `clean_sciat()` cleans and prepares the data for computing the
    SC-IAT *D*.
  - `computeD()` computes the IAT *D-score*.
  - `Dsciat()` computes the SC-IAT *D*.
  - `d_plot()`, `d_distr()` plot the results of the computation.
  - `multi_dscore()`, `multi_dsciat()` plots the results of multiple
    *D-score*s and multiple SC-IATs, respectively.

## Installation

You can install the released version of implicitMeasures from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("implicitMeasures")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("OttaviaE/implicitMeasures")
```

## Example

This is a basic example which shows you how to compute the IAT
*D-score*. More detailed examples are given the package vignettes.

``` r
library(implicitMeasures)
## load the raw_data dataframe
data("raw_data")

## prepare the dataset for the computation
iat_data <- clean_iat(raw_data, 
                          sbj_id = "Participant",
                          block_id = "blockcode",
                          mapA_practice = "practice.iat.Milkbad",
                          mapA_test = "test.iat.Milkbad",
                          mapB_practice = "practice.iat.Milkgood",
                          mapB_test = "test.iat.Milkgood",
                          latency_id = "latency",
                          accuracy_id = "correct",
                          trial_id = "trialcode",
                          trial_eliminate = c("reminder", "reminder1"))

## store the dataset for comptuing the D-score
iat <- iat_data[[1]]

## Compute the D-score
dscore <- computeD(iat, D = "d3")
```

`computeD()` results in a data frame with class `dscore`, and it can be
passe to other functions, for example for plotting the results, either
at the individual level:

<img src="man/figures/README-unnamed-chunk-2-1.png" width="100%" style="display: block; margin: auto;" />
or at the sample level:

<img src="man/figures/README-unnamed-chunk-3-1.png" width="100%" style="display: block; margin: auto;" />

## Data import

You can import import your data in any format you want. If you’re
importing the data from SPSS, please use either
`haven::read_sav("~/path/to/mydata.sav")` or
`foreign::read.spss("~/path/to/mydata.sav")` without changing the
default options of the functions. The package will recognize that the
data frame is coming form SPSS and will handle that.

## Bugs and problems

If you find any bugs or encounter any problems in using this package,
please post a minimal reproducible example on
[github](https://github.com/OttaviaE/implicitMeasures/issues). For
questions and other discussion, you can contact the author and
maintainer of the package at <otta.epifania@gmail.com>.
