
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoTox <a href="https://niehs.github.io/GeoTox/"><img src="man/figures/logo.svg" align="right" height="139" alt="GeoTox website" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/NIEHS/GeoTox/actions/workflows/check-release.yaml/badge.svg)](https://github.com/NIEHS/GeoTox/actions/workflows/check-release.yaml)
[![cov](https://NIEHS.github.io/GeoTox/badges/coverage.svg)](https://github.com/NIEHS/GeoTox/actions)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![CRAN
status](https://www.r-pkg.org/badges/version/GeoTox)](https://CRAN.R-project.org/package=GeoTox)

<!-- badges: end -->

`GeoTox` open-source R software package for characterizing the risk of
perturbing molecular targets involved in adverse human health outcomes
based on exposure to spatially-referenced stressor mixtures. The
original framework is described in

[Eccles KM, Karmaus AL, Kleinstreuer NC, Parham F, Rider CV, Wambaugh
JF, Messier KP. A geospatial modeling approach to quantifying the risk
of exposure to environmental chemical mixtures via a common molecular
target. Sci Total Environ. 2023 Jan 10;855:158905. doi:
10.1016/j.scitotenv.2022.158905. Epub 2022 Sep 21. PMID: 36152849;
PMCID: PMC9979101.](https://pubmed.ncbi.nlm.nih.gov/36152849/)

`GeoTox` package represents a significant advancement in environmental
risk characterization, providing modular software to facilitate the
application and further development of the GeoTox framework for
quantifying the relationship between environmental exposures and health
outcomes. By integrating geospatial methods with cutting-edge exposure
and toxicological frameworks, `GeoTox` offers a robust tool for
assessing individual and population-level risks from environmental
stressors.

![Source-to-Outcome Continuum concept supports the GeoTox
framework](man/figures/Exposome-Cascade.png)

## Installation

The package will be on CRAN in the near future - please stay tuned. You
can install the development version of GeoTox from
[GitHub](https://github.com/) with:

``` r
if (!require("pak", quietly = TRUE)) {
  install.packages("pak")
}
pak::pkg_install("NIEHS/GeoTox")
```
