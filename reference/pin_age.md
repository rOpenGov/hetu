# Extract Age from Personal Identity Code

Calculate age in years, months, weeks or days from personal identity
codes.

## Usage

``` r
pin_age(pin, date = Sys.Date(), timespan = "years", allow.temp = FALSE)

hetu_age(pin, date = Sys.Date(), timespan = "years", allow.temp = FALSE)
```

## Arguments

- pin:

  Finnish personal identity code(s) as a character vector

- date:

  Date at which age is calculated. If a vector is provided it must be of
  the same length as the `pin` argument.

- timespan:

  Timespan to use to calculate age. The possible timespans are:

  - `years` (Default)

  - `months`

  - `weeks`

  - `days`

- allow.temp:

  Allow artificial or temporary PINs (personal numbers 900-999). If
  `FALSE` (default), only PINs intended for official use (personal
  numbers 002-899) are allowed.

## Value

Age as an integer vector.

## Examples

``` r
ex_pin <- c("010101-0101", "111111-111C")
pin_age(ex_pin, date = "2012-01-01")
#> The age in years has been calculated at 2012-01-01.
#> [1] 111 100

ex_pin <- c("010101-0101", "111111-111C")
hetu_age(ex_pin, date = "2012-01-01")
#> The age in years has been calculated at 2012-01-01.
#> [1] 111 100
```
