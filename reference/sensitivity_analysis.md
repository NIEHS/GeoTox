# Perform sensitivity analysis

Perform sensitivity analysis

## Usage

``` r
sensitivity_analysis(x, max_mult = list(NULL, NULL, NULL, 1.2, NULL))
```

## Arguments

- x:

  GeoTox object.

- max_mult:

  numeric list of length 5 for each step of the sensitivity analysis.

## Value

The same GeoTox object with added `sensitivity` field.

## Details

This wrapper function will sequentially call the
[compute_sensitivity](https://github.com/NIEHS/GeoTox/reference/compute_sensitivity.md)
function with inputs `age`, `obesity`, `css_params`, `fit_params`, and
`C_ext`. The results will be returned as a named list and stored in the
`sensitivity` field of the input GeoTox object.

Values of `NULL` in the `max_mult` input will use the default value
stored in the `GeoTox` object (`x$par$resp$max_mult`). When a `GeoTox`
object is created this is initialized at `1.5`, but can be changed via
the
[calculate_response](https://github.com/NIEHS/GeoTox/reference/calculate_response.md)
function or directly in the object.

## See also

[`compute_sensitivity`](https://github.com/NIEHS/GeoTox/reference/compute_sensitivity.md)

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

# Sensitivity analysis can now be done
geoTox <- geoTox |> sensitivity_analysis()
```
