# Finnish personal ID number data toolkit for R (hetu)

The **hetu R package** provides tools to work with Finnish personal
identity numbers (hetu, short for the Finnish term “henkilötunnus”).
Some functions can also be used with Finnish Business ID numbers
(y-tunnus).

Where possible, we have unified the syntax with
[sweidnumbr](https://github.com/rOpenGov/sweidnumbr).

## Installation

Install the current devel version in R:

``` r
devtools::install_github("ropengov/hetu")
```

Test the installation by loading the library:

``` r
library(hetu)
```

We also recommend setting the UTF-8 encoding:

``` r
Sys.setlocale(locale = "UTF-8")
```

## Introduction

Finnish personal identification numbers (Finnish: henkilötunnus, hetu in
short), are used to identify citizens. Hetu PIN consists of eleven
characters: DDMMYYCZZZQ, where DDMMYY is the day, month and year of
birth, C is the century marker, ZZZ is the individual number and Q is
the control character.

Males have odd and females have even individual number. The control
character is determined by dividing DDMMYYZZZ by 31 and using the
remainder (modulo 31) to pick up the corresponding character from the
string “0123456789ABCDEFHJKLMNPRSTUVWXY”. For example, if the remainder
is 0, the control character is 0 and if the remainder is 12, the control
character is C.

A valid individual number is between 002-899. Individual numbers 900-999
are not in normal use and are used only for temporary or artificial
PINs. These temporary PINs are sometimes used in different
organizations, such as insurance companies or hospitals, if the
individual is not a Finnish citizen, a permanent resident or if the
exact identity of the individual cannot be determined at the time.
Artificial or temporary PINs are not intended for continuous, long term
use and they are not usually accepted by PIN validity checking
algorithms.

Temporary PINs provide similar information about individual’s birth date
or sex as regular PINs. Temporary PINs can also be safely used for
testing purposes, as such a number cannot be linked to any real person.

## Personal identification numbers (HETU)

The basic hetu function can be used to view information included in a
Finnish personal identification number. The data is outputted as a data
frame.

``` r
example_pin <- "111111-111C"
hetu(example_pin)
#>          hetu  sex p.num ctrl.char       date day month year century valid.pin
#> 1 111111-111C Male   111         C 1911-11-11  11    11 1911       -      TRUE
```

The output can be made prettier, for example by using knitr:

``` r
knitr::kable(hetu(example_pin))
```

| hetu        | sex  | p.num | ctrl.char | date       | day | month | year | century | valid.pin |
|:------------|:-----|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|
| 111111-111C | Male | 111   | C         | 1911-11-11 |  11 |    11 | 1911 | \-      | TRUE      |

The hetu function also accepts vectors with several identification
numbers as input:

``` r
example_pins <- c("010101-0101", "111111-111C")
knitr::kable(hetu(example_pins))
```

| hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin |
|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|
| 010101-0101 | Female | 010   | 1         | 1901-01-01 |   1 |     1 | 1901 | \-      | TRUE      |
| 111111-111C | Male   | 111   | C         | 1911-11-11 |  11 |    11 | 1911 | \-      | TRUE      |

The hetu function does not print warning messages to the user if input
vector contains invalid PINs. Validity of specific PINs can be
determined by looking at the valid.pin column.

``` r
hetu(c("010101-0102", "111311-111C", "010101-0101"))
#>          hetu    sex p.num ctrl.char       date day month year century
#> 1 010101-0102 Female   010         2 1901-01-01   1     1 1901       -
#> 2 111311-111C   Male   111         C       <NA>  11    NA 1911       -
#> 3 010101-0101 Female   010         1 1901-01-01   1     1 1901       -
#>   valid.pin
#> 1     FALSE
#> 2     FALSE
#> 3      TRUE
```

### Extracting specific information

Information contained in the PIN can be extracted with a generic extract
parameter. Valid values for extraction are *hetu*, *sex*,
*personal.number*, *ctrl.char*, *date*, *day*, *month*, *year*,
*century*, *valid.pin* and *is.temp*.

*is.temp* can be extracted only if allow.temp is set to TRUE. If
allow.temp is set to FALSE (default), temporary PINs are filtered from
the output and information provided by *is.temp* would be meaningless.

``` r
hetu(example_pins, extract = "sex")
#> [1] "Female" "Male"
hetu(example_pins, extract = "ctrl.char")
#> [1] "1" "C"
```

Some fields can be extracted with specialized functions. Extracting sex
with hetu_sex function:

``` r
hetu_sex(example_pins)
#> [1] "Female" "Male"
```

Extracting age at current date and at a given date with hetu_age
function:

``` r
hetu_age(example_pins)
#> The age in years has been calculated at 2026-03-10.
#> [1] 125 114
hetu_age(example_pins, date = "2012-01-01")
#> The age in years has been calculated at 2012-01-01.
#> [1] 111 100
hetu_age(example_pins, timespan = "months")
#> The age in months has been calculated at 2026-03-10.
#> [1] 1502 1371
```

Dates (birth dates) also have their own function, hetu_date.

``` r
hetu_date(example_pins)
#> [1] "1901-01-01" "1911-11-11"
```

### Validity checking

The basic hetu function output includes information on the validity of
each pin, which can be extracted by using hetu-function with *valid.pin*
as extract parameter.

The validity of the PINs can also be determined by using the hetu_ctrl
function, which produces a vector:

``` r
hetu_ctrl(c("010101-0101", "111111-111C")) # TRUE TRUE
#> [1] TRUE TRUE
hetu_ctrl("010101-1010") # FALSE
#> [1] FALSE
```

### Artificial and temporary personal identification numbers

The package functions can be made to accept artificial or temporary
personal identification numbers. Artificial and temporary PINs can be
used normally by allowing them through allow.temp parameter.

``` r
example_temp_pin <- "010101A900R"
knitr::kable(hetu(example_temp_pin, allow.temp = TRUE))
```

| hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin | is.temp |
|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|:--------|
| 010101A900R | Female | 900   | R         | 2001-01-01 |   1 |     1 | 2001 | A       | TRUE      | TRUE    |

A vector with regular and temporary PINs mixed together prints only
regular PINs, if allow.temp is not set to TRUE. Automatic omitting of
temporary PINs does not produce a visible error message and therefore
users need to be cautious if they want to use temporary PINs.

If temporary PINs are not explicitly allowed and the input vector
consists of temporary PINs only, the function will return an error.

``` r
example_temp_pins <- c("010101A900R", "010101-0101")
hetu_ctrl("010101A900R", allow.temp = FALSE)
#> [1] NA
knitr::kable(hetu(example_temp_pins))
```

|     | hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin |
|:----|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|
| 2   | 010101-0101 | Female | 010   | 1         | 1901-01-01 |   1 |     1 | 1901 | \-      | TRUE      |

When allow.temp is set to TRUE, all PINs are handled as if they were
regular PINs.

``` r
knitr::kable(hetu(example_temp_pins, allow.temp = TRUE))
```

| hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin | is.temp |
|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|:--------|
| 010101A900R | Female | 900   | R         | 2001-01-01 |   1 |     1 | 2001 | A       | TRUE      | TRUE    |
| 010101-0101 | Female | 010   | 1         | 1901-01-01 |   1 |     1 | 1901 | \-      | TRUE      | FALSE   |

``` r
hetu_ctrl("010101A900R", allow.temp = TRUE)
#> [1] TRUE
```

Validation function hetu_ctrl produces a FALSE for every artificial /
temporary PIN, if they are not explicitly allowed.

``` r
knitr::kable(hetu(example_temp_pins)) #FALSE TRUE
```

|     | hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin |
|:----|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|
| 2   | 010101-0101 | Female | 010   | 1         | 1901-01-01 |   1 |     1 | 1901 | \-      | TRUE      |

``` r
knitr::kable(hetu(example_temp_pins, allow.temp = TRUE)) #TRUE TRUE
```

| hetu        | sex    | p.num | ctrl.char | date       | day | month | year | century | valid.pin | is.temp |
|:------------|:-------|:------|:----------|:-----------|----:|------:|-----:|:--------|:----------|:--------|
| 010101A900R | Female | 900   | R         | 2001-01-01 |   1 |     1 | 2001 | A       | TRUE      | TRUE    |
| 010101-0101 | Female | 010   | 1         | 1901-01-01 |   1 |     1 | 1901 | \-      | TRUE      | FALSE   |

### Generating random PINs

Random PINs can be generated by using the rpin function.

``` r
rhetu(n = 4)
#> [1] "180323-144L" "230526-034N" "301246-7226" "080978V740D"
rhetu(n = 4, start.date = "1990-01-01", end.date = "2005-01-01")
#> [1] "270297-3423" "260399-427H" "310799-861Y" "250902A270L"
```

The number of males in the generated sample can be changed with
parameter p.male. Default is 0.4.

``` r
random_sample <- rhetu(n = 4, p.male = 0.8)
table(random_sample)
#> random_sample
#> 031021A0556 120527Y187S 160117A275C 200210E521W 
#>           1           1           1           1
```

The default proportion of artificial / temporary PINs is 0.0, meaning
that no artificial / temporary PINs are generated by default.

``` r
temp_sample <- rhetu(n = 4, p.temp = 0.5)
table(hetu(temp_sample, allow.temp = TRUE, extract = "is.temp"))
#> 
#> FALSE 
#>     4
```

### Diagnostics

In addition to information mentioned in the section [Extracting specific
information](#extracting-specific-information), the user can choose to
print additional columns containing information about checks done on
PINs. The diagnostic checks produce a TRUE or FALSE for the following
categories: *valid.p.num*, *valid.checksum*, *correct.checksum*,
*valid.date*, *valid.day*, *valid.month*, *valid.year*, *valid.length*
and *valid.century*, FALSE meaning that hetu is somehow incorrect.

``` r
diagnosis_example <- c("010101-0102", "111111-111Q",
                       "010101B0101", "320101-0101", "011301-0101",
                       "010101-01010", "010101-0011")
head(hetu(diagnosis_example, diagnostic = TRUE), 3)
#>          hetu    sex p.num ctrl.char       date day month year century
#> 1 010101-0102 Female   010         2 1901-01-01   1     1 1901       -
#> 2 111111-111Q   Male   111         Q 1911-11-11  11    11 1911       -
#> 3 010101B0101 Female   010         1 2001-01-01   1     1 2001       B
#>   valid.pin valid.p.num valid.ctrl.char correct.ctrl.char valid.date valid.day
#> 1     FALSE        TRUE            TRUE             FALSE       TRUE      TRUE
#> 2     FALSE        TRUE           FALSE             FALSE       TRUE      TRUE
#> 3      TRUE        TRUE            TRUE              TRUE       TRUE      TRUE
#>   valid.month valid.year valid.length valid.century
#> 1        TRUE       TRUE         TRUE          TRUE
#> 2        TRUE       TRUE         TRUE          TRUE
#> 3        TRUE       TRUE         TRUE          TRUE
```

Diagnostic information can be examined more closely by using subset or
by using a separate hetu_diagnostics function. The user can print all
diagnostic information for all PINs in the dataset:

``` r
tail(hetu_diagnostic(diagnosis_example), 3)
#>           hetu is.temp valid.p.num valid.ctrl.char correct.ctrl.char valid.date
#> 5  011301-0101   FALSE        TRUE            TRUE             FALSE      FALSE
#> 6 010101-01010   FALSE        TRUE            TRUE              TRUE       TRUE
#> 7  010101-0011   FALSE       FALSE            TRUE             FALSE       TRUE
#>   valid.day valid.month valid.year valid.length valid.century
#> 5      TRUE       FALSE       TRUE         TRUE          TRUE
#> 6      TRUE        TRUE       TRUE        FALSE          TRUE
#> 7      TRUE        TRUE       TRUE         TRUE          TRUE
```

By using extract parameter, the user can choose which columns will be
printed in the output table. Valid extract values are listed in the
function’s help file.

``` r
hetu_diagnostic(diagnosis_example, extract = c("valid.century",
                                               "correct.checksum"))
#> Error in `hetu_diagnostic()`:
#> ! Trying to extract invalid diagnostic(s)
```

Because of the way PINs are handled in inside hetu-function, the
diagnostics-function can show unexpected warning messages or introduce
NAs by coercion if the date-part of the PIN is too long. This may result
in inability to handle the PIN at all!

``` r
# Faulty example
hetu_diagnostic(c("01011901-01010"))
```

## Business Identity Codes (BID)

The package has also the ability to generate Finnish Business ID codes
(y-tunnus) and check their validity. Unlike with personal identification
numbers, no additional information can be extracted from Business IDs.

### Generating random BIDs

Similar to hetu PINs, random Finnish Business IDs (y-tunnus) can be
generated by using rbid function.

``` r
bid_sample <- rbid(3)
bid_sample
#> [1] "2817006-0" "5891569-2" "5462040-8"
```

### BID validity checking

The validity of Finnish Business Identity Codes can be checked with a
similar function to hetu_ctrl: bid_ctrl.

``` r
bid_ctrl(c("0737546-2", "1572860-0")) # TRUE TRUE
#> [1] TRUE TRUE
bid_ctrl("0737546-1") # FALSE
#> [1] FALSE
```

## Various examples

Data frames generated by hetu function work well with tidyverse/dplyr
workflows as well.

``` r
library(hetu)
library(tidyverse)
library(dplyr)

# Generate data for this example
hdat <- tibble(pin = rhetu(n = 4,
                           start.date = "1990-01-01",
                           end.date = "2005-01-01"))

# Extract all the hetu information to tibble format
hdat <- hdat %>%
  mutate(result = map(.x = pin, .f = hetu::hetu)) %>%
  unnest(cols = c(result))
hdat
```

## Licensing and Citations

This work can be freely used, modified and distributed under the open
license specified in the [DESCRIPTION
file](https://github.com/rOpenGov/hetu/blob/master/DESCRIPTION).

Kindly cite the work as follows

``` r
citation("hetu")
#> To cite package ‘hetu’ in publications use:
#> 
#>   Kantanen P, Bülow E, Lahtinen A, Magnusson M, Paananen J, Lahti L
#>   (2025). "Validating and Extracting Information from National
#>   Identification Numbers in R: The Case of Finland and Sweden." _The R
#>   Journal_, *16*, 4-14. ISSN 2073-4859, doi:10.32614/RJ-2024-023
#>   <https://doi.org/10.32614/RJ-2024-023>.
#> 
#> We strongly recommend citing the software used in research, as per
#> FORCE11 Software citation principles:
#> 
#>   Pyry Kantanen, Måns Magnusson, Jussi Paananen and Leo Lahti (2025).
#>   hetu: Structural Handling of Finnish Personal Identity Codes
#>   [Computer software]. R package version 1.2.0. DOI:
#>   https://doi.org/10.32614/CRAN.package.hetu
#> 
#> To see these entries in BibTeX format, use 'print(<citation>,
#> bibtex=TRUE)', 'toBibtex(.)', or set
#> 'options(citation.bibtex.max=999)'.
```

## References

- [The personal identity
  code](https://dvv.fi/en/personal-identity-code). Digital and
  population data services agency.
- [Valtioneuvoston asetus väestötietojärjestelmästä
  (128/2010)](https://www.finlex.fi/fi/laki/ajantasa/2010/20100128) (In
  Finnish). Valtiovarainministeriö.
- [HETU-uudistuksen
  loppuraportti](https://urn.fi/URN:ISBN:978-952-367-296-3) (In
  Finnish). Valtiovarainministeriön julkaisuja 2020:20.
- [The Business Information System
  (BIS)](https://www.prh.fi/en/yhdistysrekisteri/business_id.html).
  Finnish Patent and Registration Office.

## Session info

This vignette was created with

``` r
sessionInfo()
#> R version 4.5.2 (2025-10-31)
#> Platform: x86_64-pc-linux-gnu
#> Running under: Ubuntu 24.04.3 LTS
#> 
#> Matrix products: default
#> BLAS:   /usr/lib/x86_64-linux-gnu/openblas-pthread/libblas.so.3 
#> LAPACK: /usr/lib/x86_64-linux-gnu/openblas-pthread/libopenblasp-r0.3.26.so;  LAPACK version 3.12.0
#> 
#> locale:
#>  [1] LC_CTYPE=C.UTF-8       LC_NUMERIC=C           LC_TIME=C.UTF-8       
#>  [4] LC_COLLATE=C.UTF-8     LC_MONETARY=C.UTF-8    LC_MESSAGES=C.UTF-8   
#>  [7] LC_PAPER=C.UTF-8       LC_NAME=C              LC_ADDRESS=C          
#> [10] LC_TELEPHONE=C         LC_MEASUREMENT=C.UTF-8 LC_IDENTIFICATION=C   
#> 
#> time zone: UTC
#> tzcode source: system (glibc)
#> 
#> attached base packages:
#> [1] stats     graphics  grDevices utils     datasets  methods   base     
#> 
#> other attached packages:
#> [1] hetu_1.2.0
#> 
#> loaded via a namespace (and not attached):
#>  [1] vctrs_0.7.1       cli_3.6.5         knitr_1.51        rlang_1.1.7      
#>  [5] xfun_0.56         generics_0.1.4    textshaping_1.0.5 jsonlite_2.0.0   
#>  [9] glue_1.8.0        backports_1.5.0   htmltools_0.5.9   ragg_1.5.1       
#> [13] sass_0.4.10       rmarkdown_2.30    evaluate_1.0.5    jquerylib_0.1.4  
#> [17] fastmap_1.2.0     yaml_2.3.12       lifecycle_1.0.5   compiler_4.5.2   
#> [21] fs_1.6.7          timechange_0.4.0  htmlwidgets_1.6.4 systemfonts_1.3.2
#> [25] digest_0.6.39     R6_2.6.1          pillar_1.11.1     parallel_4.5.2   
#> [29] bslib_0.10.0      checkmate_2.3.4   tools_4.5.2       lubridate_1.9.5  
#> [33] pkgdown_2.2.0     cachem_1.1.0      desc_1.4.3
```
