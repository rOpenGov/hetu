# Generate Random Finnish Business ID's (Y-tunnus)

A function that generates random Finnish Business ID's, `bid`-numbers
(Y-tunnus).

## Usage

``` r
rbid(n)
```

## Arguments

- n:

  number of generated BIDs

## Value

a vector of generated `BID`-numbers.

## Examples

``` r
x <- rbid(3)
bid_ctrl(x)
#> [1] TRUE TRUE TRUE
```
