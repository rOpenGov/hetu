# *News*
==========

# hetu 1.1.0 (2024-12-03)

* Add summary method and plot methods for data.frames produced by `hetu_diagnostic()`
* Add support for the new century markers that were added in a Finnish government decree 8.7.2022/690 that amended another government decree 25.2.2010/128 ("Government Decree on the Population Information System").

# hetu 1.0.7 (2022-05-16)

* subsetting-parameter (TRUE or FALSE) dropped from `hetu_diagnostic()` function as it was unnecessary syntactic sugar that was difficult to communicate to users. Similar functionalities can be easily achieved with standard subsetting functionalities found in base R and especially in tidyverse.
* `satu_ctrl_char()` parameter for printing whole SATU/FINUID-numbers is now called "print.full" instead of "complement".

# hetu 1.0.6.9000 (2022-01-18)

* Rewritten `rpin()` function for increased speed
* Added new function `hetu_control_char()` both for internal use in other functions as well as convenience (sometimes you know the rest of the identity code and just need to determine the control character)
* Added support for checking the validity of Finnish electronic Unique Identification Numbers (SATU / FINUID). Two new functions: `satu_ctrl()` and `satu_ctrl_char()`, the former works like `hetu_ctrl()` and the latter works like abovementioned `hetu_control_char()`
* `hetu()` table column name checksum changed to more descriptive ctrl.char. The change also affects related column names in `hetu_diagnostic()`. This is to illustrate the point that Finnish personal identity code has control characters (numbers and letters) instead of check digits.

# hetu 1.0.3 (2021-07-28)

* Implementing recommendations from goodpractice (whole package) and lintr (hetu.R) for better code legibility
* Renamed internal objects to make it easier to distinguish between user input (function arguments), internal objects used in intermediate steps and output names for the data table
* New function `hetu_control_char()` for easy calculation of control characters when birth date and personal numbers are already known

# hetu 1.0.2 (2020-11-23)

* Moving away from Travis CI to GitHub Actions

# hetu 1.0.1 (2020-10-15)

* Build submitted to CRAN
* More organized vignette, hetu and BID examples separated
* Minor fixes to `hetu_diagnostic()` and `rbid()`

# hetu 0.3.1 (2020-10-14)

* Added function for generating Finnish Business IDs, `rbid()`
* Added new alias to `hetu_diagnostic()`, `pin_diagnostic()`, for better consistency with other functions
* New examples in package vignette

# hetu 0.3.0 (2020-10-13)

* Optimized functions: generating and handling a large amount of PINs is now much faster (tested with 5.5 million PINs)
* new `bid_ctrl()` function for checking the validity of Finnish Business IDs
* `hetu()` table column name personal.number changed to shorter p.num

# hetu 0.2.4 (2020-09-10)

* Option to print explicit diagnostics in hetu-function
* `hetu_diagnostic()` function for easier viewing of diagnostic information

# hetu 0.2.1-0.2.3 (2020-09-09)

* Updates to pkgdown
* Improvements to vignette
* Warning messages and new tests for invalid inputs
* Fixes in hetu.R in handling temporary / artificial pins
* Tests for deprecated pin_to_date function
* Added codecov integration
* Minor fixes

# hetu 0.2.0 (2020-09-04)

* Added `rpin()` function for generating random hetu PINs
* Added ability to handle temporary or artificial hetu PINs (allow.temp, is.temp)
* Added tests to achieve 100 % test coverage
* Deprecation of pin_to_date, addition of `pin_date()`
* Numerous minor bugfixes and additions to help files and vignettes

# hetu 0.1.1 (2012-06-26)

* Package created based on earlier functions migrated from sorvi
