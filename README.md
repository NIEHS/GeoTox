
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
conc <- 10^rep(-2:2, each = 3)
tp   <- 100 # top asymptote
ga   <- 1.6 # AC50
gw   <- 1.2 # slope
resp <- tp / (1 + (ga / conc)^gw) + rnorm(length(conc), sd = 5)

fit_2param <- fit_hill(log10(conc), resp) # slope fixed at 1
fit_3param <- fit_hill(log10(conc), resp, fixed_slope = FALSE)

rbind(
  "inputs"  = c(tp, log10(ga), gw, NA),
  "3-param" = c(fit_3param$par),
  "2-param" = c(fit_2param$par[1:2], 1, fit_2param$par[3])
)
#>                tp   logAC50    slope  t-error
#> inputs  100.00000 0.2041200 1.200000       NA
#> 3-param  97.63578 0.1516374 1.485236 1.177891
#> 2-param 101.53494 0.2360917 1.000000 1.417567
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
  conc <- 10^rep(-2:2, each = 3)
  tp   <- 100 + rnorm(1, sd = 15)
  ga   <- 10^(2 * runif(1) - 1)
  gw   <- 1 + rnorm(1)/5
  resp <- tp / (1 + (ga / conc)^gw) + rnorm(length(conc))
  resp[resp < 0] <- 0
  data.frame(
    logc = log10(conc),
    resp = resp
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
C_ss <- matrix(runif(n_chem * MC_iter), nrow = MC_iter)
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
#> 1  1.316500e-04 1.316519e-04 1.197871e-05 1.197851e-05
#> 2  1.928066e-04 1.928077e-04 1.756148e-05 1.756120e-05
#> 3  1.509427e-04 1.509448e-04 1.339909e-05 1.339880e-05
#> 4  1.931396e-04 1.931407e-04 1.742350e-05 1.742319e-05
#> 5  2.000661e-04 2.000678e-04 1.748640e-05 1.748673e-05
#> 6  9.090331e-05 9.090415e-05 8.634445e-06 8.634337e-06
#> 7  6.336773e-05 6.336758e-05 5.870044e-06 5.869961e-06
#> 8  1.501012e-04 1.501024e-04 1.365014e-05 1.364991e-05
#> 9  7.009742e-05 7.009701e-05 6.962928e-06 6.962838e-06
#> 10 7.310190e-05 7.310279e-05 6.848001e-06 6.847910e-06
```
