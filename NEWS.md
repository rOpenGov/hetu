# *News*
==========

# hetu 1.0.1 (2020-10-15)

* Build submitted to CRAN
* More organized vignette, hetu and BID examples separated
* Minor fixes to hetu_diagnostic and rbid

# hetu 0.3.1 (2020-10-14)

* Added function for generating Finnish Business IDs, rbid
* Added new alias to hetu_diagnostic, pin_diagnostic, for consistent behaviour with other functions
* New examples in package vignette

# hetu 0.3.0 (2020-10-13)

* Optimized functions: generating and handling a large amount of PINs is now much faster (tested with 5.5 million PINs)
* new bid_ctrl function for checking the validity of Finnish Business IDs
* personal.number changed to shorter p.num

# hetu 0.2.4 (2020-09-10)

* Option to print explicit diagnostics in hetu-function
* hetu_diagnostic function for easier viewing of diagnostic information

# hetu 0.2.1-0.2.3 (2020-09-09)

* Updates to pkgdown
* Improvements to vignette
* Warning messages and new tests for invalid inputs
* Fixes in hetu.R in handling temporary / artificial pins
* Tests for deprecated pin_to_date function
* Added codecov integration
* Minor fixes

# hetu 0.2.0 (2020-09-04)

* Added rpin function for generating random hetu PINs
* Added ability to handle temporary or artificial hetu PINs (allow.temp, is.temp)
* Added tests to achieve 100 % test coverage
* Deprecation of pin_to_date, addition of pin_date
* Numerous minor bugfixes and additions to help files and vignettes

# hetu 0.1.1 (2012-06-26)

* Package created based on earlier functions migrated from sorvi
