
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GeoToxPackage

<!-- badges: start -->

[![test-coverage](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/test-coverage.yaml)
[![codecov](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/graph/badge.svg?token=I1L9BZJ58Y)](https://codecov.io/github/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage)
[![R-CMD-check](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/check-release.yaml/badge.svg)](https://github.com/Spatiotemporal-Exposures-and-Toxicology/GeoToxPackage/actions/workflows/check-release.yaml)
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

## Example

``` r
library(GeoToxPackage)
library(dplyr, warn.conflicts = FALSE)
```

### Estimate chemical concentration-response curves

``` r
log10_conc <- rep(-2:2, each = 3)
tp <- 10  # top asymptote
gw <- 1.2 # slope
ga <- 1.6 # AC50
resp <- tp / (1 + 10^(gw * (log10(ga) - log10_conc))) + rnorm(length(log10_conc))

fit_2param <- fit_hill(log10_conc, resp)
fit_3param <- fit_hill(log10_conc, resp, fixed_slope = FALSE)

rbind(
  "inputs"  = c(tp, log10(ga), gw, NA),
  "3-param" = c(fit_3param$par),
  "2-param" = c(fit_2param$par[1:2], 1, fit_2param$par[3])
)
#>                tp       logAC50    slope    t-error
#> inputs  10.000000  0.2041199827 1.200000         NA
#> 3-param  9.427044 -0.0255136610 7.481381 -0.5788686
#> 2-param 10.078431 -0.0009174594 1.000000 -0.1283037
```

### Estimate population dose-response

Input data

``` r
# Number of samples to simulate
MC_iter <- 10

# Number of chemicals to simulate
n_chem <- 4

# Create age groups and group sizes
age <- data.frame(
  AGEGRP = 0:18,
  TOT_POP = c(0, round(runif(18, max = 1000)))
)
age$TOT_POP[1] <- sum(age$TOT_POP[-1])

# Create chemical exposure mean and sd
exposure <- data.frame(
  mean = (1 + runif(n_chem))*1e-6,
  sd   = (1 + runif(n_chem))*1e-7
)

# Create chemical concentration-response data
conc_resp <- lapply(1:n_chem, function(idx) {
  log10_conc <- rep(-2:2, each = 3)
  tp <- 10 + rnorm(1, sd = 2)
  gw <- 1 + rnorm(1)/5
  log10_ga <- 2 * runif(1) - 1
  y <- tp / (1 + 10^(gw * (log10_ga - log10_conc)))
  data.frame(
    logc = log10_conc,
    resp = y + rnorm(length(log10_conc))
  )
})
fits <- lapply(conc_resp, function(df) {
  fit_hill(df$logc, df$resp)
})
chem_params <- do.call(
  rbind,
  lapply(fits, function(fit) {
    as_tibble(t(unlist(fit))) %>%
      rename(
        tp         = par.tp,
        tp.sd      = sds.tp,
        logAC50    = par.logAC50,
        logAC50.sd = sds.logAC50
      ) %>%
      select(
        tp, tp.sd, logAC50, logAC50.sd,
        logc_min, logc_max, resp_min, resp_max, AIC
      ) %>%
      mutate(across(tp:AIC, ~ as.numeric(.x)))
  })
)

# Steady-state concentration (will be generated from httk)
C_ss <- matrix(runif(nrow(exposure) * MC_iter), nrow = MC_iter)
```

Simulate data

``` r
# Simulate age based on relative population group sizes
simulated_age <- simulate_age(
  age,
  n = MC_iter
)

# Simulate inhalation rate using default params
simulated_IR <- simulate_inhalation_rate(
  simulated_age
)

# Simulate external exposure
simulated_exposure <- simulate_exposure(
  exposure$mean,
  exposure$sd,
  n = MC_iter
)
```

Computations

``` r
internal_dose <- calc_internal_dose(
  C_ext = simulated_exposure,
  IR = simulated_IR
)

invitro_concentration <- calc_invitro_concentration(
  D_int = internal_dose,
  C_ss = C_ss
)

concentration_response <- calc_concentration_response(
  resp = chem_params,
  concentration = invitro_concentration
)

concentration_response
#>         GCA.Eff       IA.eff    GCA.HQ.10     IA.HQ.10
#> 1  2.023560e-06 2.023554e-06 1.706002e-06 1.705980e-06
#> 2  6.929693e-06 6.929700e-06 5.431601e-06 5.431528e-06
#> 3  5.840383e-06 5.840341e-06 4.464539e-06 4.464465e-06
#> 4  1.173735e-05 1.173759e-05 8.623322e-06 8.623484e-06
#> 5  7.772241e-06 7.772348e-06 5.632912e-06 5.632988e-06
#> 6  1.063841e-05 1.063850e-05 8.026201e-06 8.026049e-06
#> 7  4.703982e-06 4.704007e-06 3.873856e-06 3.873805e-06
#> 8  9.397205e-06 9.397296e-06 7.190710e-06 7.190593e-06
#> 9  7.505428e-06 7.505503e-06 5.650852e-06 5.650743e-06
#> 10 8.195456e-06 8.195506e-06 6.092902e-06 6.093040e-06
```
