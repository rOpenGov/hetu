Finnish open government data toolkit for R
==========================================

This **hetu R package** provides tools to work with Finnish personal
identity numbers.

The syntax of this package is unified with the similar package for
Swedish ID numbers, the
[sweidnumbr](https://github.com/rOpenGov/sweidnumbr).

Installation
------------

Install the current devel version in R:

    devtools::install_github("ropengov/hetu")

Test the installation by loading the library:

    library(hetu)

We also recommend setting the UTF-8 encoding:

    Sys.setlocale(locale="UTF-8") 

Personal identification number (HETU)
-------------------------------------

At simplest, we can use the following to extract information from a
Finnish personal identification number.

    hetu("111111-111C")

    ##          hetu gender personal.number checksum       date day month year
    ## 1 111111-111C   Male             111        C 1911-11-11  11    11 1911
    ##   century.char
    ## 1            -

The function accepts also vectors as input, returning a data frame:

    example_pin <- c("010101-0101", "111111-111C")
    knitr::kable(hetu(example_pin))

<table>
<thead>
<tr class="header">
<th style="text-align: left;">hetu</th>
<th style="text-align: left;">gender</th>
<th style="text-align: right;">personal.number</th>
<th style="text-align: left;">checksum</th>
<th style="text-align: left;">date</th>
<th style="text-align: right;">day</th>
<th style="text-align: right;">month</th>
<th style="text-align: right;">year</th>
<th style="text-align: left;">century.char</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">010101-0101</td>
<td style="text-align: left;">Female</td>
<td style="text-align: right;">10</td>
<td style="text-align: left;">1</td>
<td style="text-align: left;">1901-01-01</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1</td>
<td style="text-align: right;">1901</td>
<td style="text-align: left;">-</td>
</tr>
<tr class="even">
<td style="text-align: left;">111111-111C</td>
<td style="text-align: left;">Male</td>
<td style="text-align: right;">111</td>
<td style="text-align: left;">C</td>
<td style="text-align: left;">1911-11-11</td>
<td style="text-align: right;">11</td>
<td style="text-align: right;">11</td>
<td style="text-align: right;">1911</td>
<td style="text-align: left;">-</td>
</tr>
</tbody>
</table>

**Validate Finnish personal identification number:**

    valid_hetu("010101-0101") # TRUE/FALSE

    ## [1] TRUE

    pin_ctrl("010101-0101") # TRUE/FALSE - same function name as in sweidnumbr

    ## [1] TRUE

### Extracting specific fields

General field extraction from personal identification number.

    hetu(example_pin, extract = "gender")

    ## [1] "Female" "Male"

Extracting sex:

    pin_sex(example_pin)

    ## [1] Female Male  
    ## Levels: Female Male

Extracting age:

    pin_age(example_pin)

    ## [1] 19  8

    pin_age(example_pin, date = "2020-01-01")

    ## [1] 19  8

Licensing and Citations
-----------------------

This work can be freely used, modified and distributed under the open
license specified in the [DESCRIPTION
file](https://github.com/MansMeg/hetu/blob/master/DESCRIPTION).

Kindly cite the work as follows

    citation("hetu")

    ## 
    ## Kindly cite the hetu R package as follows:
    ## 
    ##   (C) Leo Lahti, Mans Magnusson and Jussi Paananen (rOpenGov 2020).
    ##   hetu: Finnish personal ID number data toolkit for R.  URL:
    ##   http://github.com/rOpenGov/hetu
    ## 
    ## A BibTeX entry for LaTeX users is
    ## 
    ##   @Misc{,
    ##     title = {hetu: Finnish personal ID number data toolkit for R},
    ##     author = {Leo Lahti and Mans Magnusson and Jussi Paananen},
    ##     year = {2020},
    ##   }
    ## 
    ## Many thanks for all contributors!

Session info
------------

This vignette was created with

    sessionInfo()

    ## R version 4.0.0 (2020-04-24)
    ## Platform: x86_64-pc-linux-gnu (64-bit)
    ## Running under: Ubuntu 20.04 LTS
    ## 
    ## Matrix products: default
    ## BLAS:   /home/lei/bin/R-4.0.0/lib/libRblas.so
    ## LAPACK: /home/lei/bin/R-4.0.0/lib/libRlapack.so
    ## 
    ## locale:
    ##  [1] LC_CTYPE=en_US.UTF-8       LC_NUMERIC=C              
    ##  [3] LC_TIME=en_US.UTF-8        LC_COLLATE=en_US.UTF-8    
    ##  [5] LC_MONETARY=en_US.UTF-8    LC_MESSAGES=en_US.UTF-8   
    ##  [7] LC_PAPER=en_US.UTF-8       LC_NAME=C                 
    ##  [9] LC_ADDRESS=C               LC_TELEPHONE=C            
    ## [11] LC_MEASUREMENT=en_US.UTF-8 LC_IDENTIFICATION=C       
    ## 
    ## attached base packages:
    ## [1] stats     graphics  grDevices utils     datasets  methods   base     
    ## 
    ## other attached packages:
    ## [1] hetu_0.1.1     testthat_2.3.2 rmarkdown_2.3  knitr_1.29    
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] Rcpp_1.0.4.6      compiler_4.0.0    highr_0.8         prettyunits_1.1.1
    ##  [5] remotes_2.1.1     tools_4.0.0       digest_0.6.25     pkgbuild_1.0.8   
    ##  [9] pkgload_1.1.0     lubridate_1.7.9   evaluate_0.14     memoise_1.1.0    
    ## [13] checkmate_2.0.0   rlang_0.4.6       rstudioapi_0.11   cli_2.0.2        
    ## [17] yaml_2.2.1        xfun_0.15         withr_2.2.0       stringr_1.4.0    
    ## [21] fs_1.4.1          generics_0.0.2    desc_1.2.0        devtools_2.3.0   
    ## [25] rprojroot_1.3-2   glue_1.4.1        R6_2.4.1          processx_3.4.2   
    ## [29] fansi_0.4.1       sessioninfo_1.1.1 callr_3.4.3       magrittr_1.5     
    ## [33] backports_1.1.8   ps_1.3.3          ellipsis_0.3.1    htmltools_0.5.0  
    ## [37] usethis_1.6.1     assertthat_0.2.1  stringi_1.4.6     crayon_1.3.4
