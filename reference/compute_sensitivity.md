# Compute response sensitivity to parameter variation.

Compute response sensitivity to parameter variation.

## Usage

``` r
compute_sensitivity(
  x,
  vary = c("age", "obesity", "css_params", "fit_params", "C_ext"),
  max_mult = NULL
)
```

## Arguments

- x:

  GeoTox object.

- vary:

  which parameter to vary.

- max_mult:

  input for
  [calc_concentration_response](https://github.com/NIEHS/GeoTox/reference/calc_concentration_response.md)
  step.

## Value

output from
[calc_concentration_response](https://github.com/NIEHS/GeoTox/reference/calc_concentration_response.md)

## Examples

``` r
# Use a subset of the package data for demonstration purposes
set.seed(2357)
n <- 10 # Population size
m <- 5 # Number of regions
idx <- if (m < 100) sample(1:100, m) else 1:100

# Create GeoTox object and populate required fields
geoTox <- GeoTox() |>
  # Simulate populations for each region
  simulate_population(age = split(geo_tox_data$age, ~FIPS)[idx],
                      obesity = geo_tox_data$obesity[idx, ],
                      exposure = split(geo_tox_data$exposure, ~FIPS)[idx],
                      simulated_css = geo_tox_data$simulated_css,
                      n = n) |>
  # Estimated Hill parameters
  set_hill_params(geo_tox_data$dose_response |>
                    fit_hill(assay = "endp", chem = "casn") |>
                    dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed))

# Sensitivity computations can now be done
age_resp <- geoTox |> compute_sensitivity()
obesity_resp <- geoTox |> compute_sensitivity(vary = "obesity")
```
