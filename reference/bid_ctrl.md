# Check Validity of Finnish Business ID (Y-tunnus)

A function that checks whether a `bid` (Finnish Business ID) is valid.
Returns `TRUE` or `FALSE`.

## Usage

``` r
bid_ctrl(bid)
```

## Arguments

- bid:

  a vector of 1 or more business identity numbers

## Examples

``` r
bid_ctrl(c("0000000-0", "0000001-9")) # TRUE TRUE
#> [1] TRUE TRUE
bid_ctrl("0737546-1") # FALSE
#> [1] FALSE
```
