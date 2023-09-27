# avoncap: Functions for manipulating and analysing the AvonCap study data.


<!-- badges: start -->

[![R-CMD-check](https://github.com/bristol-vaccine-centre/avoncap/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bristol-vaccine-centre/tableone/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/538909567.svg)](https://zenodo.org/badge/latestdoi/538909567)
[![avoncap status
badge](https://bristol-vaccine-centre.r-universe.dev/badges/avoncap)](https://bristol-vaccine-centre.r-universe.dev)
<!-- badges: end -->

This is a development status package, designed only to support AvonCap publications with and will never be submitted to CRAN.

It is under continuous revision and use of stable releases is recommended

## Installation

This package is hosted
in the [Bristol Vaccine Centre r-universe](https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install avoncap in R
install.packages("avoncap")
```

Installation can be performed through devtools:

```R
install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/avoncap")

# recommended to use a stable release version such as:
devtools::install_github("bristol-vaccine-centre/avoncap@0.0.0.9000")
```


Bump version
