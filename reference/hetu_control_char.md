# Calculate Control Character for Personal Identity Code

Calculate a valid control character for an incomplete Finnish personal
identity codes (hetu).

## Usage

``` r
hetu_control_char(pin, with.century = TRUE)
```

## Arguments

- pin:

  An incomplete PIN that ONLY has a date, century marker (optional, see
  parameter with.century) and personal number

- with.century:

  If TRUE (default), the function assumes that the PIN input contains a
  century marker (DDMMYYQZZZ). If FALSE, the function assumes that the
  PIN contains only date and personal number (DDMMYYZZZ).

## Value

Control character, either a number 0-9 or a letter.

## Details

This method of calculating the control character was devised by
mathematician Erkki Pale (1962) to detect input errors but also to
detect errors produced by early punch card machines. The long number
produced by writing the birth date and the personal number together are
divided by 31 and the remainder is used to look up the control character
from a separate table containing alphanumeric characters except letters
G, I, O, Q and Z.

The method of calculating the control character does not need century
character and therefore the function has an option to omit it.

## See also

[`hetu`](https://ropengov.github.io/hetu/reference/hetu.md) For
extracting information from Finnish personal identity codes.

## Author

Pyry Kantanen

## Examples

``` r
hetu_control_char("010101-010")
#> [1] "1"
hetu_control_char("010101010", with.century = FALSE)
#> [1] "1"
```
