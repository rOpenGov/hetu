
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](https://ropengov.org/)
[![R build
status](https://github.com/rOpenGov/hetu/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/hetu/actions)
[![codecov](https://codecov.io/gh/rOpenGov/hetu/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rOpenGov/hetu)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/hetu)](https://cran.r-project.org/package=hetu)
[![Downloads](http://cranlogs.r-pkg.org/badges/hetu)](https://cran.r-project.org/package=hetu)
[![Watch on
GitHub](https://img.shields.io/github/watchers/ropengov/hetu.svg?style=social)](https://github.com/ropengov/hetu/watchers)
[![Star on
GitHub](https://img.shields.io/github/stars/ropengov/hetu.svg?style=social)](https://github.com/ropengov/hetu/stargazers)
[![Follow](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![cran
version](http://www.r-pkg.org/badges/version/hetu)](https://CRAN.R-project.org/package=hetu)
<!-- badges: end -->

<!--[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/hetu)](https://github.com/metacran/cranlogs.app)-->

# hetu - Structural Handling of Finnish Personal Identity Numbers <a href="https://ropengov.github.io/hetu/"><img src="man/figures/logo.png" align="right" height="139" /></a>

### Introduction

`hetu` is an R package for structural handling of identification codes
used in Finland, most importantly Finnish flavour of national
identification numbers, the Finnish personal identity codes (in Finnish:
henkilötunnus (hetu), in Swedish: personbeteckning). Some functions can
also be used with Finnish Business ID numbers (in Finnish: yritys- ja
yhteisötunnus (y-tunnus), in Swedish: företags- och organisationsnummer
(FO-nummer)) and Finnish Unique Identification Numbers (FINUID, in
Finnish: sähköinen asiointitunnus (SATU), in Swedish: elektronisk
kommunikationskod).

The syntax in this package aims to be, when convenient, similar with
another package specializing in national identification numbers: the
[sweidnumbr](https://github.com/rOpenGov/sweidnumbr) R package for
working with Swedish personal identity numbers and corporation identity
numbers.

### Installation

Install stable version from CRAN:

``` r
install.packages(hetu)
```

Alternatively, use `devtools` package to install the latest development
version from GitHub:

``` r
devtools::install_github("rOpenGov/hetu")
```

Development version can be also installed using the
[r-universe](https://ropengov.r-universe.dev):

``` r
# Enable this universe
options(repos = c(
  ropengov = "https://ropengov.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
install.packages("hetu")
```

### Loading the package and accessing the tutorial

Loading the package in R command line:

``` r
library(hetu)
```

A simple example of printing a table containing data from 2 imaginary
personal identity codes:

``` r
example_hetu <- c("010101-0101", "111111-111C")
hetu(example_hetu)
#>          hetu    sex p.num ctrl.char       date day month year century
#> 1 010101-0101 Female   010         1 1901-01-01   1     1 1901       -
#> 2 111111-111C   Male   111         C 1911-11-11  11    11 1911       -
#>   valid.pin
#> 1      TRUE
#> 2      TRUE
```

The package can also be used to check the validity of Finnish Business
IDs (Yritys- ja Yhteisötunnus, or Y-tunnus for short):

``` r
example_ytunnus <- c("5996039-9", "5619117-6", "6095515-9")
bid_ctrl(example_ytunnus)
#> [1] TRUE TRUE TRUE
```

A tutorial is included with the package and can be viewed with
vignette-function:

``` r
vignette("hetu")
```

## Contributing

  - [Submit suggestions and bug
    reports](https://github.com/ropengov/hetu/issues) (provide the
    output of `sessionInfo()` and `packageVersion("hetu")` and
    preferably provide a [reproducible
    example](http://adv-r.had.co.nz/Reproducibility.html))
  - [Send a pull request](https://github.com/rOpenGov/hetu/pulls)
  - [Star us on the Github page](https://github.com/ropengov/hetu/)
  - [See our website](https://ropengov.org/community/) for additional
    contact information

## Acknowledgements

**Kindly cite this work** as follows: [Pyry
Kantanen](https://github.com/pitkant/), Måns Magnusson, Jussi Paananen,
Leo Lahti. hetu: Finnish personal ID number data toolkit for R. URL:
<https://ropengov.github.io/hetu/>

We are grateful to all
[contributors](https://github.com/rOpenGov/hetu/graphs/contributors)\!
This project is part of [rOpenGov](https://ropengov.org).
