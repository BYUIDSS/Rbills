
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Rbills

<!-- badges: start -->

<!-- badges: end -->

## Overview

`Rbills` helps you scrape information from PDF files. It is desined to
scrape only `Rocky Mountain Power` and `Summit Energy` documents.

You can use the `Rbills` package to:

  - **get\_pdf**: get all files in a directory or get files that you
    choose in a directory.

  - **read\_pdf\_seg**: read pdf files from Summit Energy documents.

  - **read\_pdf\_rmp**: read pdf files from Rocky Mountain Power
    documents.

Theses functions are for use only with the data team and no other
people.

## Installation

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("BYUIDSS/Rbills")
```

## Usage

``` r
library(Rbills)
```

### get\_pdf

There are two main ways to use `get_pdf` function:

  - To get all files in a directory, insert path in the function.
    `choose.file = FALSE` is default. The following code is an example.

<!-- end list -->

``` r
path <- system.file("data-raw", package = "Rbills", mustWork = TRUE)
get_pdf(path)
```

  - To get files that you choose in a directory, insert path and
    `choose.file = TRUE`. The following code is an example.

<!-- end list -->

``` r
path <- system.file("data-raw", package = "Rbills", mustWork = TRUE)
get_pdf(path, choose.file = TRUE)
```

### read\_pdf\_seg

The `read_pdf_seg` function only works for `Summit Energy` documents.
This function provides a table of data that fits the template of a given
file. The following code is an example.

``` r
path <- system.file("data-raw", package = "Rbills", mustWork = TRUE)

# Choose 'example_gasbill' file.
x <- get_pdf(path, choose.file = TRUE)

gas_table <- read_pdf_seg(path, x)

head(gas_table)
#>   meter_id billing_month invoice_date gas_supp_price gas_supp_mmbtu
#> 1 24800033      May 2013 June 5, 2013           2.28           1056
#>   gas_supp_ext trans_fuel_price trans_fuel_mmbtu trans_fuel_ext
#> 1      2407.68              2.1               37           77.7

```

### read\_pdf\_rmp

The `read_pdf_rmp` function only works for `Rocky Mountain Power`
documents. This function provides a table of data that fits the template
of a given file. The following code is an example.

``` r
path <- system.file("data-raw", package = "Rbills", mustWork = TRUE)

# Choose 'example_powerbill' file.
x <- get_pdf(path, choose.file = TRUE)

# Type Bldg 85, Main Bldg, East Bldg, West Bldg 1, West Bldg 2 when prompt function occurs.
power_table <- read_pdf_rmp(path, x)
#> Please enter one or more building names, separated by commas and ensure names match the building names on the bill:

head(power_table)
#> [1] invoice_date building     meter_number onkwh        offkwh      
#> [6] totalkwh     kvarh       
#> <0 rows> (or 0-length row.names)

```
