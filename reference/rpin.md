# Generate Random Personal Identity Codes

A function that generates random Finnish personal identity codes (`hetu`
codes).

## Usage

``` r
rpin(
  n,
  start.date = as.Date("1895-01-01"),
  end.date = Sys.Date(),
  p.male = 0.4,
  p.temp = 0,
  num.cores = 1
)

rhetu(
  n,
  start.date = as.Date("1895-01-01"),
  end.date = Sys.Date(),
  p.male = 0.4,
  p.temp = 0,
  num.cores = 1
)
```

## Arguments

- n:

  number of generated `hetu`-pins

- start.date:

  Lower limit of generated `hetu` dates, character string in ISO 8601
  standard, for example "2001-02-03". Default is "1895-01-01".

- end.date:

  Upper limit of generated `hetu`. Default is current date.

- p.male:

  Probability of males, between 0.0 and 1.0. Default is 0.4.

- p.temp:

  Probability of temporary identification numbers, between 0.0 and 1.0.
  Default is 0.0.

- num.cores:

  The number of cores for parallel processing. The number of available
  cores can be determined with `detectCores()`. Default is 1.

## Value

a vector of generated `hetu`-pins.

## Details

This function will return an error "too few positive probabilities" in
[`sample.int`](https://rdrr.io/r/base/sample.html) function if you try
to generate too many codes in a short enough timeframe. The theoretical
upper limit of valid PINs is in the millions, but the number of valid
PINs per day used to be 898 PINs at maximum, meaning 327770 for each
year. Attempting to generate e.g. a 1000 pins for a timespan of one day
would result in an error.

In practice this theoretical upper limit number was much lower since the
old practice was that the same personal number component cannot be
"recycled" if it has been used in the past. To illustrate, if an
identity code "010101-0101" has already been assigned to someone born in
1901-01-01, a similar code "010101A0101" for someone born in 2001-01-01
could not be used.

In hetu package version 1.1.0 we have taken into account a new
government decree that increased the amount of valid century markers and
therefore increased the amount of valid personal codes per day.
Additionally, the decree has made it possible to recycle individual
codes, as the century marker is now thought to be a distinguishing
character of the personal identity code.

However, the current implementation still keeps the old 898 codes per
day limit intact, and assigns new century markers with a low
probability: old markers "-" and "A" are given a 95 markers are given a
1

In the future this may be altered into a waterfall pattern so that the
initial 898 codes for each date get "-" as the century marker, the next
898 get "Y", and so on. This would mean that each day would have 5388
valid codes and the distribution of century markers would be more
realistic in the sense that additional century markers are taken into
use only after the previous range has been exhausted. However, this
would require generating rather large datasets even for basic testing
purposes.

## Author

Pyry Kantanen, Jussi Paananen

## Examples

``` r
x <- rpin(3)
hetu(x)
#>          hetu    sex p.num ctrl.char       date day month year century
#> 1 020433-2215   Male   221         5 1933-04-02   2     4 1933       -
#> 2 170147-462T Female   462         T 1947-01-17  17     1 1947       -
#> 3 101002A0333   Male   033         3 2002-10-10  10    10 2002       A
#>   valid.pin
#> 1      TRUE
#> 2      TRUE
#> 3      TRUE
hetu(x, extract = "sex")
#> [1] "Male"   "Female" "Male"  
hetu(x, extract = "ctrl.char")
#> [1] "5" "T" "3"

x <- rhetu(3)
x
#> [1] "130334-674A" "200438-883D" "230379-412X"
```
