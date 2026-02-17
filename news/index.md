# Changelog

## hetu 1.2.0 (2025-06-24)

- Edit package citation to include a reference to the joint article in R
  Journal describing the `hetu` and `sweidnumbr` packages

## hetu 1.1.0 (2024-12-03)

CRAN release: 2024-12-04

- Add summary method and plot methods for data.frames produced by
  [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md)
- Add support for the new century markers that were added in a Finnish
  government decree 8.7.2022/690 that amended another government decree
  25.2.2010/128 (“Government Decree on the Population Information
  System”).

## hetu 1.0.7 (2022-05-16)

CRAN release: 2022-05-21

- subsetting-parameter (TRUE or FALSE) dropped from
  [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md)
  function as it was unnecessary syntactic sugar that was difficult to
  communicate to users. Similar functionalities can be easily achieved
  with standard subsetting functionalities found in base R and
  especially in tidyverse.
- `satu_ctrl_char()` parameter for printing whole SATU/FINUID-numbers is
  now called “print.full” instead of “complement”.

## hetu 1.0.6.9000 (2022-01-18)

- Rewritten
  [`rpin()`](https://ropengov.github.io/hetu/reference/rpin.md) function
  for increased speed
- Added new function
  [`hetu_control_char()`](https://ropengov.github.io/hetu/reference/hetu_control_char.md)
  both for internal use in other functions as well as convenience
  (sometimes you know the rest of the identity code and just need to
  determine the control character)
- Added support for checking the validity of Finnish electronic Unique
  Identification Numbers (SATU / FINUID). Two new functions:
  [`satu_ctrl()`](https://ropengov.github.io/hetu/reference/satu_ctrl.md)
  and `satu_ctrl_char()`, the former works like
  [`hetu_ctrl()`](https://ropengov.github.io/hetu/reference/pin_ctrl.md)
  and the latter works like abovementioned
  [`hetu_control_char()`](https://ropengov.github.io/hetu/reference/hetu_control_char.md)
- [`hetu()`](https://ropengov.github.io/hetu/reference/hetu.md) table
  column name checksum changed to more descriptive ctrl.char. The change
  also affects related column names in
  [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md).
  This is to illustrate the point that Finnish personal identity code
  has control characters (numbers and letters) instead of check digits.

## hetu 1.0.3 (2021-07-28)

- Implementing recommendations from goodpractice (whole package) and
  lintr (hetu.R) for better code legibility
- Renamed internal objects to make it easier to distinguish between user
  input (function arguments), internal objects used in intermediate
  steps and output names for the data table
- New function
  [`hetu_control_char()`](https://ropengov.github.io/hetu/reference/hetu_control_char.md)
  for easy calculation of control characters when birth date and
  personal numbers are already known

## hetu 1.0.2 (2020-11-23)

- Moving away from Travis CI to GitHub Actions

## hetu 1.0.1 (2020-10-15)

CRAN release: 2020-10-24

- Build submitted to CRAN
- More organized vignette, hetu and BID examples separated
- Minor fixes to
  [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md)
  and [`rbid()`](https://ropengov.github.io/hetu/reference/rbid.md)

## hetu 0.3.1 (2020-10-14)

- Added function for generating Finnish Business IDs,
  [`rbid()`](https://ropengov.github.io/hetu/reference/rbid.md)
- Added new alias to
  [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md),
  [`pin_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md),
  for better consistency with other functions
- New examples in package vignette

## hetu 0.3.0 (2020-10-13)

- Optimized functions: generating and handling a large amount of PINs is
  now much faster (tested with 5.5 million PINs)
- new
  [`bid_ctrl()`](https://ropengov.github.io/hetu/reference/bid_ctrl.md)
  function for checking the validity of Finnish Business IDs
- [`hetu()`](https://ropengov.github.io/hetu/reference/hetu.md) table
  column name personal.number changed to shorter p.num

## hetu 0.2.4 (2020-09-10)

- Option to print explicit diagnostics in hetu-function
- [`hetu_diagnostic()`](https://ropengov.github.io/hetu/reference/hetu_diagnostic.md)
  function for easier viewing of diagnostic information

## hetu 0.2.1-0.2.3 (2020-09-09)

- Updates to pkgdown
- Improvements to vignette
- Warning messages and new tests for invalid inputs
- Fixes in hetu.R in handling temporary / artificial pins
- Tests for deprecated pin_to_date function
- Added codecov integration
- Minor fixes

## hetu 0.2.0 (2020-09-04)

- Added [`rpin()`](https://ropengov.github.io/hetu/reference/rpin.md)
  function for generating random hetu PINs
- Added ability to handle temporary or artificial hetu PINs (allow.temp,
  is.temp)
- Added tests to achieve 100 % test coverage
- Deprecation of pin_to_date, addition of
  [`pin_date()`](https://ropengov.github.io/hetu/reference/pin_date.md)
- Numerous minor bugfixes and additions to help files and vignettes

## hetu 0.1.1 (2012-06-26)

- Package created based on earlier functions migrated from sorvi
