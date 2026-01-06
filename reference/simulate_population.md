# Simulate population data

Simulate population data for given input fields

## Usage

``` r
simulate_population(
  x,
  age = NULL,
  obesity = NULL,
  exposure = NULL,
  simulated_css = NULL,
  ...
)
```

## Arguments

- x:

  GeoTox object.

- age:

  input `x` to function
  [simulate_age](https://github.com/NIEHS/GeoTox/reference/simulate_age.md).
  After simulating ages, the inhalation rate is subsequently calculated
  using
  [simulate_inhalation_rate](https://github.com/NIEHS/GeoTox/reference/simulate_inhalation_rate.md).

- obesity:

  input `x` to function
  [simulate_obesity](https://github.com/NIEHS/GeoTox/reference/simulate_obesity.md).

- exposure:

  input `x` to function
  [simulate_exposure](https://github.com/NIEHS/GeoTox/reference/simulate_exposure.md).

- simulated_css:

  input `simulated_css` to functions
  [sample_Css](https://github.com/NIEHS/GeoTox/reference/sample_Css.md)
  and
  [get_fixed_css](https://github.com/NIEHS/GeoTox/reference/get_fixed_css.md).

- ...:

  additional arguments passed to other functions. See details.

## Value

The same object with simulated fields added.

## Details

Additional parameters include `n` for sample size(s), `IR_params` for
[simulate_inhalation_rate](https://github.com/NIEHS/GeoTox/reference/simulate_inhalation_rate.md),
`obes_prev`, `obes_sd`, and `obes_label` for
[simulate_obesity](https://github.com/NIEHS/GeoTox/reference/simulate_obesity.md),
and `expos_mean`, `expos_sd`, and `expos_label` for
[simulate_exposure](https://github.com/NIEHS/GeoTox/reference/simulate_exposure.md).

## Examples

``` r
# Use a subset of the package data for demonstration purposes
set.seed(2357)
n <- 10 # Population size
m <- 5 # Number of regions
idx <- if (m < 100) sample(1:100, m) else 1:100

# Create GeoTox object
geoTox <- GeoTox() |>
  # Simulate populations for each region
  simulate_population(age = split(geo_tox_data$age, ~FIPS)[idx],
                      obesity = geo_tox_data$obesity[idx, ],
                      exposure = split(geo_tox_data$exposure, ~FIPS)[idx],
                      simulated_css = geo_tox_data$simulated_css,
                      n = n)
                      
# Variable population sizes
n <- 6:10
geoTox <- GeoTox() |>
  # Simulate populations for each region
  simulate_population(age = split(geo_tox_data$age, ~FIPS)[idx],
                      obesity = geo_tox_data$obesity[idx, ],
                      exposure = split(geo_tox_data$exposure, ~FIPS)[idx],
                      simulated_css = geo_tox_data$simulated_css,
                      n = n)
```
