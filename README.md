<!-- badges: start -->
  [![Build Status](https://travis-ci.org/rOpenGov/hetu.svg?branch=master)](https://travis-ci.org/rOpenGov/hetu)
  [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/rOpenGov/hetu?branch=master&svg=true)](https://ci.appveyor.com/project/rOpenGov/hetu)
  [![codecov](https://codecov.io/gh/rOpenGov/hetu/branch/master/graph/badge.svg)](https://codecov.io/gh/rOpenGov/hetu)
  [![Watch on GitHub][github-watch-badge]][github-watch]
  [![Star on GitHub][github-star-badge]][github-star]
  [![Follow](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
  <!-- badges: end -->

<!--[![Coverage Status](https://coveralls.io/repos/github/rOpenGov/hetu/badge.svg?branch=master)](https://coveralls.io/github/rOpenGov/hetu?branch=master)-->

<!--[![rstudio mirror downloads](http://cranlogs.r-pkg.org/badges/grand-total/hetu)](https://github.com/metacran/cranlogs.app)-->

<!--[![cran version](http://www.r-pkg.org/badges/version/hetu)](http://cran.rstudio.com/web/packages/hetu)-->

hetu
==========

## Introduction

`hetu` is an R package for structural handling of identity numbers used in the Finnish administration, in particular the personal identity numbers (henkilötunnus). Some functions can also be used with Finnish Business ID numbers (y-tunnus).

The syntax in this package is unified with the similar package for Swedish ID numbers, the [sweidnumbr](https://github.com/rOpenGov/sweidnumbr).


## Installation

To install from CRAN just write:

```r
install.packages(hetu)
```

Use the `devtools` package to install the latest version from GitHub:
```r
devtools::install_github("rOpenGov/hetu")
library(hetu)
```

A tutorial is included with the package and can be viewed with:
```r
vignette("hetu")
```

## Reporting bugs

Please use the GitHub issue tracker [here](https://github.com/rOpenGov/hetu/issues) for reporting bugs and making further feature requests.

IMPORTANT: When submitting a bug, you can make the lives of the developers easier by submitting the following information along with your bug report:
- The output of `sessionInfo()`
- The output of `packageVersion("hetu")`


[github-watch-badge]: https://img.shields.io/github/watchers/ropengov/hetu.svg?style=social
[github-watch]: https://github.com/ropengov/hetu/watchers
[github-star-badge]: https://img.shields.io/github/stars/ropengov/hetu.svg?style=social
[github-star]: https://github.com/ropengov/hetu/stargazers
