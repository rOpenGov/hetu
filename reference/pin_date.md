# Extract Date of Birth from Personal Identity Code

Returns the date of birth in date format.

## Usage

``` r
pin_date(pin, allow.temp = FALSE)

hetu_date(pin, allow.temp = FALSE)
```

## Arguments

- pin:

  Finnish personal identity code(s) as a character vector

- allow.temp:

  Allow artificial or temporary PINs (personal numbers 900-999). If
  `FALSE` (default), only PINs intended for official use (personal
  numbers 002-899) are allowed.

## Value

Date of birth as a vector in date format.

## Examples

``` r
pin_date(c("010101-0101", "111111-111C"))
#> [1] "1901-01-01" "1911-11-11"

hetu_date(c("010101-0101", "111111-111C"))
#> [1] "1901-01-01" "1911-11-11"
```
