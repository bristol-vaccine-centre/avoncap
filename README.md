# avoncap: Functions for manipulating and analysing the AvonCap study data.

This is a development status package, designed only to support AvonCap publications with and will never be submitted to CRAN.

It is under continuous revision and use of stable releases is recommended

## Installation

This package is hosted
in the [Bristol Vaccine Centre
r-universe](https://https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install tableone in R
install.packages("avoncap")
```

Installation can be performed through devtools:

```R
install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/avoncap")

# recommended to use a stable release version such as:
devtools::install_github("bristol-vaccine-centre/avoncap@0.0.0.9000")
```
