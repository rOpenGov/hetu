# Generic Extraction Tool for Finnish Personal Identity Codes

Extract embedded information from Finnish personal identity codes
(hetu).

## Usage

``` r
hetu(
  pin,
  extract = NULL,
  allow.temp = FALSE,
  diagnostic = FALSE,
  as.factor = FALSE
)
```

## Arguments

- pin:

  Finnish personal identity code(s) as a character vector

- extract:

  Extract only selected part of the information. Valid values are
  "`hetu`", "`sex`", "`p.num`", "`ctrl.char`", "`date`", "`day`",
  "`month`", "`year`", "`century`", "`is.temp`". If `NULL` (default),
  returns all information.

- allow.temp:

  Allow artificial or temporary PINs (personal numbers 900-999). If
  `FALSE` (default), only PINs intended for official use (personal
  numbers 002-899) are allowed.

- diagnostic:

  Print additional information about possible problems in PINs. The
  checks are "`valid.p.num`", "`valid.ctrl.char`",
  "`correct.ctrl.char`", "`valid.date`", "`valid.day`", "`valid.month`",
  "`valid.length`", "`valid.century`". Default is `FALSE` which returns
  no diagnostic information.

- as.factor:

  Makes fields "`sex`", "`p.num`", "`ctrl.char`" and "`century`" into
  factors for slightly reduced memory footprint. Default is FALSE.

## Value

Finnish personal identity code data.frame, or if extract parameter is
set, the requested part of the information as a vector. Returns an error
or `NA` if the given character vector is not a valid Finnish personal
identity code.

- hetu:

  Finnish personal identity code as a character vector. A correct pin
  should be in the form DDMMYYCZZZQ, where DDMMYY stands for date, C for
  century sign, ZZZ for personal number and Q for control character.

- sex:

  sex of the person as a character vector ("Male" or "Female")

- p.num:

  Personal number (individual number) part of the identity code

- ctrl.char:

  Control character for the personal identity code

- date:

  Birthdate

- day:

  Day of the birthdate

- month:

  Month of the birthdate

- year:

  Year of the birthdate

- century:

  Century character determining the century (1800s, 1900s or 2000s) of
  the person's birth. See details for more information

- valid.pin:

  Does the personal identity code pass all validity checks: (`TRUE` or
  `FALSE`)

## Details

Starting from 1st of January 2023, an amendment to the government decree
on the Population Information System (128/2010) has expanded the number
of available century markers (See references: Valtioneuvoston asetus
VM/2022/124) and scrapped some old practices.

For the users of this package the most visible change will be that
people born in the 1900s can now be assigned with "Y", "X", "W", "V" or
"U", in addition to the old "-" (slash) marker. People born in the 2000s
can be assigned with "B", "C", "D", E" or "F", in addition to the old
marker, "A". For people born in the 1800s "+" (plus sign) remains the
only valid marker. The amendment does not affect already existing
personal identity codes.

The change was done to mitigate for the diminishing pool of available,
unique identity codes. For historical reasons, the century marker of the
code was not always taken into account when determining the uniqueness
of the number. This meant that individual number parts were not recycled
between people born in different centuries, diminishing the amount of
available numbers for people born in the new century. For example, if a
female born in the 1st of January 1901 was assigned with the personal
identity code "010101-0101" (individual code part "010"), a female born
in 1st of January 2001 could not be assigned with the code "010101A0101"
because it would contain the same individual code as the person born in
1901 and individual codes could not be recycled. With the amended decree
the uniqueness of the personal identity code is considered by looking at
the personal identity code as a whole. This means that from now on it
would be permissible to have personal identity codes such as
"100190-999P" and "100190Y999P" at the same time, denoting two different
individuals (see references: Digital and population data services agency
announcement).

In practice, codes with new separators will be issued only when the
ranges ranges with currently used separators run out. This means that it
might take a while until we see people born in the 2000s assigned with
the century marker "C" or people born in the 1900s assigned with the
century marker "X", as there are still plenty of numbers in ranges "B"
and "Y" as well, in addition to some numbers being left in the original
ranges of "A" and "-". The first personal identity code with a new
separator "Y" was assigned in December 2023 (see Digi- ja
väestötietovirasto 2023).

The result of all this is that the hetu package may now give
"unrealistic" personal identity codes in the sense that some codes are
not yet actually in use. However, it is not the aim of this package to
simulate the actual distributions of personal identity codes and their
century markers in the population (the actually used and unused codes
are unknown to us), but to provide a tool that can be used to extract
data from these codes, should the user encounter them at some point.
Writing further sanity checks is probably a good idea for people who are
interested in detecting unusual patterns in their databases and
registries.

## References

Valtioneuvoston asetus VM/2022/124 [Valtioneuvoston asetus
VM/2022/124](https://vm.fi/paatos?decisionId=0900908f807c5f3c)

Digi- ja väestötietovirasto. (2023). [Uudet välimerkit takaavat
henkilötunnusten riittävyyden - ensimmäinen uudenlainen henkilötunnus
myönnettiin tällä
viikolla](https://dvv.fi/-/uudet-valimerkit-takaavat-henkilotunnusten-riittavyyden-ensimmainen-uudenlainen-henkilotunnus-myonnettiin-talla-viikolla)

Digital and Population Data Services Agency. [Reform of the separators
in the personal identity
code](https://dvv.fi/en/reform-of-personal-identity-code)

## See also

[`pin_ctrl`](https://ropengov.github.io/hetu/reference/pin_ctrl.md)
Validating Finnish personal identity codes.
[`rhetu`](https://ropengov.github.io/hetu/reference/rpin.md) Generating
random Finnish personal identity codes.

## Author

Pyry Kantanen, Jussi Paananen

## Examples

``` r
hetu("111111-111C")
#>          hetu  sex p.num ctrl.char       date day month year century valid.pin
#> 1 111111-111C Male   111         C 1911-11-11  11    11 1911       -      TRUE
hetu("111111-111C")$date
#> [1] "1911-11-11"
hetu("111111-111C")$sex
#> [1] "Male"
# Same as previous, but using extract argument
hetu("111111-111C", extract="sex")
#> [1] "Male"
# Process a vector of hetu's
hetu(c("010101-0101", "111111-111C"))
#>          hetu    sex p.num ctrl.char       date day month year century
#> 1 010101-0101 Female   010         1 1901-01-01   1     1 1901       -
#> 2 111111-111C   Male   111         C 1911-11-11  11    11 1911       -
#>   valid.pin
#> 1      TRUE
#> 2      TRUE
# Process a vector of hetu's and extract sex information from each
hetu(c("010101-0101", "111111-111C"), extract="sex")
#> [1] "Female" "Male"  
# Process codes with new century markers
new_codes <- c("010594Y9032", "010594Y9021", "020594X903P")
hetu(new_codes)
#> [1] NA
```
