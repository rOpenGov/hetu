
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
  reports](https://github.com/ropengov/hetu/issues) (provide the output
  of `sessionInfo()` and `packageVersion("hetu")` and preferably provide
  a [reproducible example](http://adv-r.had.co.nz/Reproducibility.html))
- [Send a pull request](https://github.com/rOpenGov/hetu/pulls)
- [Star us on the Github page](https://github.com/ropengov/hetu/)
- [See our website](https://ropengov.org/community/) for additional
  contact information

## Acknowledgements

**Kindly cite this work** as follows (citing the related paper strongly
preferred but we encourage also citing software used in research):

``` text
> print(citation("hetu"), bibtex=TRUE)
To cite package ‘hetu’ in publications use:

  Kantanen P, Bülow E, Lahtinen A, Magnusson M, Paananen J, Lahti L
  (2025). "Validating and Extracting Information from National
  Identification Numbers in R: The Case of Finland and Sweden." _The R
  Journal_, *16*, 4-14. ISSN 2073-4859, doi:10.32614/RJ-2024-023
  <https://doi.org/10.32614/RJ-2024-023>.

A BibTeX entry for LaTeX users is

  @Article{RJ-2024-023,
    title = {Validating and Extracting Information from National Identification Numbers in R: The Case of Finland and Sweden},
    author = {Pyry Kantanen and Erik B{\"u}low and Aleksi Lahtinen and M{\aa}ns Magnusson and Jussi Paananen and Leo Lahti},
    year = {2025},
    doi = {10.32614/RJ-2024-023},
    journal = {The R Journal},
    volume = {16},
    issue = {3},
    issn = {2073-4859},
    pages = {4-14},
  }

We strongly recommend citing the software used in research, as per
FORCE11 Software citation principles:

  Pyry Kantanen, Måns Magnusson, Jussi Paananen and Leo Lahti (2025).
  hetu: Structural Handling of Finnish Personal Identity Codes
  [Computer software]. R package version 1.2.0. DOI:
  https://doi.org/10.32614/CRAN.package.hetu

A BibTeX entry for LaTeX users is

  @Misc{,
    title = {hetu: Structural Handling of Finnish Personal Identity Codes},
    author = {Pyry Kantanen and M{\aa}ns Magnusson and Jussi Paananen and Leo Lahti},
    doi = {10.32614/CRAN.package.hetu},
    url = {https://github.com/rOpenGov/hetu},
    year = {2025},
    note = {R package version 1.2.0},
  }
```

We are grateful to all
[contributors](https://github.com/rOpenGov/hetu/graphs/contributors)!
This project is part of [rOpenGov](https://ropengov.org).
