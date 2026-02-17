# Finnish Unique Identification Number Control Character Calculator

Calculate a valid control character for an incomplete Finnish Unique
Identification Number (FINUID, or sähköinen asiointitunnus SATU).

## Usage

``` r
satu_control_char(pin, print.full = FALSE)
```

## Arguments

- pin:

  An incomplete FINUID that has 8 first numbers.

- print.full:

  Should the function print only the whole FINUID-number (TRUE) or only
  the control character (FALSE). Default is FALSE.

## Value

Control character, either a number 0-9 or a letter (length 1 character).
If parameter print.full is set to TRUE, the function returns a complete
FINUID / SATU number (length 9 characters).

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

For more detailed information about FINUID, see Finnish Digital and
population data services agency website:
<https://dvv.fi/en/citizen-certificate-and-electronic-identity>

## Author

Pyry Kantanen

## Examples

``` r
# The first assigned FINUID number, 10000001N.
satu_control_char("10000001")
#> [1] "N"
```
