# Check Validity of Personal Identity Code

Validate Finnish personal identity codes (hetu).

## Usage

``` r
pin_ctrl(pin, allow.temp = FALSE)

hetu_ctrl(pin, allow.temp = FALSE)
```

## Arguments

- pin:

  Finnish personal identity code(s) as a character vector

- allow.temp:

  If TRUE, temporary PINs (personal numbers 900-999) are handled
  similarly to regular PINs (personal numbers 002-899), meaning that
  otherwise valid temporary PIN will return a TRUE. Default is `FALSE`.

## Value

A logical vector indicating whether the input vector contains valid
Finnish personal identity codes.

## See also

[`hetu`](https://ropengov.github.io/hetu/reference/hetu.md) For
extracting information from Finnish personal identity codes.

## Author

Pyry Kantanen

## Examples

``` r
pin_ctrl("010101-0101") # TRUE
#> [1] TRUE
pin_ctrl("010101-010A") # FALSE
#> [1] FALSE
pin_ctrl(c("010101-0101", "010101-010A")) # TRUE FALSE
#> [1]  TRUE FALSE
hetu_ctrl("010101-0101") # TRUE
#> [1] TRUE
hetu_ctrl("010101-010A") # FALSE
#> [1] FALSE
hetu_ctrl(c("010101-0101", "010101-010A")) # TRUE FALSE
#> [1]  TRUE FALSE
```
