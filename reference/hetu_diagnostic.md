# Diagnostics Tool for Personal Identity Codes

Prints information on the tests that are used to confirm or reject the
validity of each personal identity code.

## Usage

``` r
hetu_diagnostic(pin, extract = NULL)

pin_diagnostic(pin, extract = NULL)
```

## Arguments

- pin:

  Finnish personal identification number as a character vector, or
  vector of identification numbers as a character vectors

- extract:

  Extract only selected part of the diagnostic information. Valid values
  are "`hetu`", "`is.temp`", "`valid.p.num`", "`valid.ctrl.char`",
  "`correct.ctrl.char`", "`valid.date`", "`valid.day`", "`valid.month`",
  "`valid.length`", "`valid.century`". If `NULL` (default), returns all
  information.

## Value

A data.frame containing diagnostic checks about PINs.

## See also

[`hetu`](https://ropengov.github.io/hetu/reference/hetu.md) for the main
function on which `hetu_diagnostic` relies on.

## Examples

``` r
diagnosis_example <- c("010101-0102", "111111-111Q",
"010101B0101", "320101-0101", "011301-0101",
"010101-01010", "010101-0011", "010101-9011", "010101-901S")
## Print all diagnostics for various fake personal identity codes
hetu_diagnostic(diagnosis_example)
#>           hetu is.temp valid.p.num valid.ctrl.char correct.ctrl.char valid.date
#> 1  010101-0102   FALSE        TRUE            TRUE             FALSE       TRUE
#> 2  111111-111Q   FALSE        TRUE           FALSE             FALSE       TRUE
#> 3  010101B0101   FALSE        TRUE            TRUE              TRUE       TRUE
#> 4  320101-0101   FALSE        TRUE            TRUE              TRUE      FALSE
#> 5  011301-0101   FALSE        TRUE            TRUE             FALSE      FALSE
#> 6 010101-01010   FALSE        TRUE            TRUE              TRUE       TRUE
#> 7  010101-0011   FALSE       FALSE            TRUE             FALSE       TRUE
#> 8  010101-9011    TRUE        TRUE            TRUE             FALSE       TRUE
#> 9  010101-901S    TRUE        TRUE            TRUE              TRUE       TRUE
#>   valid.day valid.month valid.year valid.length valid.century
#> 1      TRUE        TRUE       TRUE         TRUE          TRUE
#> 2      TRUE        TRUE       TRUE         TRUE          TRUE
#> 3      TRUE        TRUE       TRUE         TRUE          TRUE
#> 4     FALSE        TRUE       TRUE         TRUE          TRUE
#> 5      TRUE       FALSE       TRUE         TRUE          TRUE
#> 6      TRUE        TRUE       TRUE        FALSE          TRUE
#> 7      TRUE        TRUE       TRUE         TRUE          TRUE
#> 8      TRUE        TRUE       TRUE         TRUE          TRUE
#> 9      TRUE        TRUE       TRUE         TRUE          TRUE
# Extract century-related checks
hetu_diagnostic(diagnosis_example, extract = "valid.century")
#>           hetu valid.century
#> 1  010101-0102          TRUE
#> 2  111111-111Q          TRUE
#> 3  010101B0101          TRUE
#> 4  320101-0101          TRUE
#> 5  011301-0101          TRUE
#> 6 010101-01010          TRUE
#> 7  010101-0011          TRUE
#> 8  010101-9011          TRUE
#> 9  010101-901S          TRUE
# Print a summary in natural language
summary(hetu_diagnostic(diagnosis_example))
#> Diagnostics for 9 hetu objects: 
#> Number of valid hetu objects: 2 
#> Number of valid and non-temporary* hetu objects: 1 
#> Number of valid and temporary** hetu objects: 1 
#> 
#> Number of invalid hetu objects: 7 
#> Number of invalid and non-temporary* hetu objects: 6 
#> Number of invalid and temporary** hetu objects: 1 
#> 
#>  * non-temporary: p.num in range [002-899]
#>  ** temporary: p.num in range [900-999]
#> 
diagnosis_example <- c("010101-0102", "111111-111Q",
"010101B0101", "320101-0101", "011301-0101",
"010101-01010", "010101-0011")
## Print all diagnoses
pin_diagnostic(diagnosis_example)
#>           hetu is.temp valid.p.num valid.ctrl.char correct.ctrl.char valid.date
#> 1  010101-0102   FALSE        TRUE            TRUE             FALSE       TRUE
#> 2  111111-111Q   FALSE        TRUE           FALSE             FALSE       TRUE
#> 3  010101B0101   FALSE        TRUE            TRUE              TRUE       TRUE
#> 4  320101-0101   FALSE        TRUE            TRUE              TRUE      FALSE
#> 5  011301-0101   FALSE        TRUE            TRUE             FALSE      FALSE
#> 6 010101-01010   FALSE        TRUE            TRUE              TRUE       TRUE
#> 7  010101-0011   FALSE       FALSE            TRUE             FALSE       TRUE
#>   valid.day valid.month valid.year valid.length valid.century
#> 1      TRUE        TRUE       TRUE         TRUE          TRUE
#> 2      TRUE        TRUE       TRUE         TRUE          TRUE
#> 3      TRUE        TRUE       TRUE         TRUE          TRUE
#> 4     FALSE        TRUE       TRUE         TRUE          TRUE
#> 5      TRUE       FALSE       TRUE         TRUE          TRUE
#> 6      TRUE        TRUE       TRUE        FALSE          TRUE
#> 7      TRUE        TRUE       TRUE         TRUE          TRUE
```
