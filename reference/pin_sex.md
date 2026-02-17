# Extract Sex from Personal Identity Code

Extract sex (as binary) from Finnish personal identification code.

## Usage

``` r
pin_sex(pin, allow.temp = TRUE)

hetu_sex(pin, allow.temp = TRUE)
```

## Arguments

- pin:

  Finnish personal identity code(s) as a character vector

- allow.temp:

  Allow artificial or temporary PINs (personal numbers 900-999). If
  `FALSE` (default), only PINs intended for official use (personal
  numbers 002-899) are allowed.

## Value

Factor with label 'Male' and 'Female'.

## See also

[`hetu`](https://ropengov.github.io/hetu/reference/hetu.md) For general
information extraction

## Author

Pyry Kantanen, Leo Lahti

## Examples

``` r
pin_sex("010101-010A")
#> [1] "Female"
hetu_sex("010101-010A")
#> [1] "Female"
```
