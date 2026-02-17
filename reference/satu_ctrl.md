# Check Validity of Finnish Unique Identification Number (SATU)

A function that checks whether a `satu` (Finnish Unique Identification
Number) is valid. Returns `TRUE` or `FALSE`.

## Usage

``` r
satu_ctrl(satu)
```

## Arguments

- satu:

  a vector of 1 or more Unique Identification Numbers

## Examples

``` r
satu_ctrl("10000001N") # TRUE
#> [1] TRUE
satu_ctrl(c("10000001N", "20000001B")) # TRUE FALSE
#> [1]  TRUE FALSE
```
