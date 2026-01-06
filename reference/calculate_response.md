# Calculate response

Calculate mixture response for GeoTox population data

## Usage

``` r
calculate_response(x, ...)
```

## Arguments

- x:

  GeoTox object

- ...:

  additional arguments passed to other functions. See details.

## Value

The same object with additional fields added or updated

## Details

Additional parameters include `time`, `BW`, and `scaling` for
[calc_internal_dose](https://github.com/NIEHS/GeoTox/reference/calc_internal_dose.md),
and `max_mult` for
[calc_concentration_response](https://github.com/NIEHS/GeoTox/reference/calc_concentration_response.md).

## See also

[calc_internal_dose](https://github.com/NIEHS/GeoTox/reference/calc_internal_dose.md),
[calc_invitro_concentration](https://github.com/NIEHS/GeoTox/reference/calc_invitro_concentration.md),
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

# Response computations can now be done
geoTox <- geoTox |> calculate_response()
```
