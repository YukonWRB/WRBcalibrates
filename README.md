
<!-- README.md is generated from README.Rmd. Please edit that file -->

# WRBcalibrates

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- badges: end -->

WRBcalibrates is a package to aid the Yukon Water Resources Branch in
collecting and storing calibration data.

## Installation

You can install the development version of WRBcalibrates from GitHub
using the code below. Note however that the repository is private;
you’ll need to be a member of YukonWRB or have a token issued for you by
a member.

``` r
devtools::install.github("YukonWRB/WRBcalibrates")
```

Stable/production versions of the package are available on the G drive
at: G:\_GW_SW-packages.

## Data storage back-end

The app is designed to work with either Google Sheets or with an SQL
back-end. The package contains functions to create a local .SQLite
database (db_create()), and the function calConnect() can be used to
connect to the database. The logic behind creating a connection function
instead of simply using DBI::dbConnect() is that, if the database type
changes or if options need to be specified for the database connection,
the default parameters of calConnect() can be changed and will
automatically apply to every connection made via calConnect. Once
connected, the database type does not matter as long as it accepts
standard SQL language and is relational.
