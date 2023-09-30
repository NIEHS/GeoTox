
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoToxPackage

<!-- badges: start -->
[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/graph/badge.svg?token=I1L9BZJ58Y)](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/GeoToxPackage)](https://CRAN.R-project.org/package=GeoToxPackage)

<!-- badges: end -->

The GeoToxPackage can <x y z plus other stuff>, as introduced in [Eccles
KM, Karmaus AL, Kleinstreuer NC, Parham F, Rider CV, Wambaugh JF,
Messier KP. A geospatial modeling approach to quantifying the risk of
exposure to environmental chemical mixtures via a common molecular
target. Sci Total Environ. 2023 Jan 10;855:158905. doi:
10.1016/j.scitotenv.2022.158905. Epub 2022 Sep 21. PMID: 36152849;
PMCID: PMC9979101.](https://pubmed.ncbi.nlm.nih.gov/36152849/)

## Installation

You can install the development version of GeoToxPackage from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage")
```

## Code Coverage
![Code Coverage Visualization](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/graphs/sunburst.svg?token=I1L9BZJ58Y)


## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(GeoToxPackage)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.
