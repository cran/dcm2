
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dcm2

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![License](https://img.shields.io/badge/License-MIT-blue.svg)

[![R-CMD-check](https://github.com/atlas-aai/dcm2/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atlas-aai/dcm2/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/atlas-aai/dcm2/branch/main/graph/badge.svg)](https://app.codecov.io/gh/atlas-aai/dcm2?branch=main)
<!-- badges: end -->

The goal of dcm2 is to provide a generic function for calculating the M2
statistic described by [Liu et
al. (2016)](https://doi.org/10.3102/1076998615621293). The package
provides native support for models estimated with GDINA, but package
authors can create methods for different classes of models.

## Installation

You can install the release version of {dcm2} from
[CRAN](https://cran.r-project.org/):

``` r
install.packages("dcm2")
```

To install the development version from [GitHub](https://github.com/)
use:

``` r
# install.packages("remotes")
remotes::install_github("atlas-aai/dcm2")
```
