
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](http://ropengov.org/)
[![R build
status](https://github.com/rOpenGov/hetu/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/hetu/actions)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/rOpenGov/hetu?branch=master&svg=true)](https://ci.appveyor.com/project/rOpenGov/hetu)
[![codecov](https://codecov.io/gh/rOpenGov/hetu/branch/master/graph/badge.svg)](https://codecov.io/gh/rOpenGov/hetu)
[![Downloads](http://cranlogs.r-pkg.org/badges/grand-total/hetu)](https://cran.r-project.org/package=hetu)
[![Downloads](http://cranlogs.r-pkg.org/badges/hetu)](https://cran.r-project.org/package=hetu)
[![Watch on
GitHub](https://img.shields.io/github/watchers/ropengov/hetu.svg?style=social)](https://github.com/ropengov/hetu/watchers)
[![Star on
GitHub](https://img.shields.io/github/stars/ropengov/hetu.svg?style=social)](https://github.com/ropengov/hetu/stargazers)
[![Follow](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![cran
version](http://www.r-pkg.org/badges/version/hetu)](http://cran.rstudio.com/web/packages/hetu)
<!-- badges: end -->

<!--[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/hetu)](https://github.com/metacran/cranlogs.app)-->

# hetu - Structural Handling of Finnish Personal Identity Numbers <a href="https://ropengov.github.io/hetu/"><img src="man/figures/logo.png" align="right" height="139" /></a>

### Introduction

`hetu` is an R package for structural handling of national
identification numbers used in Finland, or more specifically Finnish
personal identity codes (in Finnish: henkilötunnus (hetu), in Swedish:
personbeteckning). Some functions can also be used with Finnish Business
ID numbers (y-tunnus).

The syntax in this package is unified with the similar package for
Swedish ID numbers, the
[sweidnumbr](https://github.com/rOpenGov/sweidnumbr).

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

### Loading the package and accessing the tutorial

Load the package:

``` r
library(hetu)
```

A tutorial is included with the package and can be viewed with
vignette-function:

``` r
example_hetu <- c("010101-0101", "111111-111C")
hetu(example_hetu)
#>          hetu    sex p.num checksum       date day month year century valid.pin
#> 1 010101-0101 Female   010        1 1901-01-01   1     1 1901       -      TRUE
#> 2 111111-111C   Male   111        C 1911-11-11  11    11 1911       -      TRUE
```

### Reporting bugs

Please use the GitHub issue tracker
[here](https://github.com/rOpenGov/hetu/issues) for reporting bugs and
making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the
developers easier by submitting the following information along with
your bug report: \* The output of `sessionInfo()` \* The output of
`packageVersion("hetu")`
