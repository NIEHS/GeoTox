# Simulate population characteristics and exposures

Simulate age, weight category, exposure rates, and external exposures.
Sample from pre-simulated steady-state plasma concentrations.

## Usage

``` r
simulate_population(
  GT,
  age = NULL,
  obesity = NULL,
  exposure = NULL,
  simulate_rate = TRUE,
  sample_css = TRUE,
  ...
)
```

## Arguments

- GT:

  GeoTox object.

- age:

  Data frame with age data.

- obesity:

  Data frame with obesity data.

- exposure:

  Data frame with exposure data.

- simulate_rate:

  Logical indicating whether to simulate exposure rates. This requires
  that exposure rate parameters have been added using
  [`add_exposure_rate_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure_rate_params.md).

- sample_css:

  Logical indicating whether to sample steady-state plasma
  concentrations (C\\\_{ss}\\). This requires that a table of simulated
  C\\\_{ss}\\ values has been set using
  [`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md).
  In addition,
  [`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md)
  will be called after sampling to prepare C\\\_{ss}\\ values for
  sensitivity analysis.

- ...:

  Additional arguments passed to wrapped functions (see 'Additional
  arguments' section of 'Details').

## Value

The updated GeoTox object, invisibly.

## Details

This is a wrapper around several other functions:

- [`add_age()`](https://github.com/NIEHS/GeoTox/dev/reference/add_age.md)
  and
  [`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md)
  for age simulation.

- [`add_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/add_obesity.md)
  and
  [`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md)
  for weight category simulation.

- [`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md)
  and
  [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md)
  for external exposure concentration simulation (C\\\_{ext}\\).

- [`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md)
  for exposure rate simulation.

- [`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md)
  for sampling steady-state plasma concentrations (C\\\_{ss}\\).

- [`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md)
  to prepare C\\\_{ss}\\ values for sensitivity analysis.

The user can provide data frames for age, obesity, and exposure data; if
any of these are provided, the corresponding add and simulate functions
will be called. The user can also specify whether to simulate exposure
rates and sample C\\\_{ss}\\ values using the `simulate_rate` and
`sample_css` arguments, respectively. If `simulate_rate` is `TRUE`,
exposure rate parameters must have been added using
[`add_exposure_rate_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure_rate_params.md).
If `sample_css` is `TRUE`, a table of simulated C\\\_{ss}\\ values must
already exist using
[`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md).

### Additional arguments:

- n:

  Number of samples to simulate (default 1000). Used in
  [`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md),
  [`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md),
  and
  [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md).
  Ignored if the 'sample' table already exists, in which case the
  existing sample sizes are used.

- location:

  Column name for location ID (default "FIPS"). Used in
  [`add_age()`](https://github.com/NIEHS/GeoTox/dev/reference/add_age.md),
  [`add_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/add_obesity.md),
  and
  [`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md).

- overwrite:

  Logical indicating whether to overwrite existing values (default
  FALSE). Used in
  [`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md),
  [`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md),
  [`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md),
  and
  [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md).

- substance:

  Column name for substance ID (default "casn"). Used in
  [`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md).

- route:

  Column name for exposure route (default "route"). Used in
  [`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md).

- rate_extra_cols:

  Additional columns to include in exposure_rate table. Used in
  [`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md).

- obes_prev, obes_sd:

  Column names for obesity prevalence and standard deviation (default
  "OBESITY_CrudePrev" and "OBESITY_SD", respectively). Used in
  [`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md).

- expos_mean, expos_sd:

  Column names for exposure concentration mean and standard deviation
  (default "mean" and "sd", respectively). Used in
  [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md).

- css_extra_cols:

  Additional columns to include in simulated_css table. Used in
  [`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md).

- substance_order:

  Named list specifying order of substances. Used in
  [`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md)
  and
  [`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md).

## See also

[`add_age()`](https://github.com/NIEHS/GeoTox/dev/reference/add_age.md),
[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md),
[`add_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/add_obesity.md),
[`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md),
[`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md),
[`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md),
[`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md),
[`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md),
[`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md)

## Examples

``` r
# Example simulation data

age_df <- data.frame(
  FIPS = rep(c(10000, 20000), each = 19),
  AGEGRP = rep(0:18, times = 2),
  TOT_POP = 0
)
# FIPS 10000, populate age group 40-44
age_df$TOT_POP[c(1, 10)] = 100
# FIPS 20000, populate age groups 50-59
age_df$TOT_POP[c(1, 12, 13) + 19] = c(200, 100, 100)

obesity_df <- data.frame(
  FIPS = c(10000, 20000),
  OBESITY_CrudePrev = c(20, 80),
  OBESITY_SD = 5
)

exposure_df <- tibble::tribble(
  ~FIPS, ~casn, ~route, ~mean, ~sd,
  10000, "00-00-1", "inhalation", 10, 1,
  10000, "00-00-2", "inhalation", 20, 1,
  20000, "00-00-1", "inhalation", 30, 1,
  20000, "00-00-2", "inhalation", 40, 1
)

# Note: normally the css_df would have many more rows for each combination of
# the non-'css' columns to allow for sampling.
css_df <- tibble::tribble(
  ~casn, ~age_lb, ~age_ub, ~weight, ~css,
  "00-00-1",  0, 49, "Normal",  1,
  "00-00-1", 50, 99, "Normal",  2,
  "00-00-1",  0, 49,  "Obese", 11,
  "00-00-1", 50, 99,  "Obese", 12,
  "00-00-2",  0, 49, "Normal", 21,
  "00-00-2", 50, 99, "Normal", 22,
  "00-00-2",  0, 49,  "Obese", 31,
  "00-00-2", 50, 99,  "Obese", 32
)

# Simulate population
GT <- GeoTox() |>
  add_exposure_rate_params() |>
  set_simulated_css(css_df) |>
  simulate_population(
    age = age_df,
    obesity = obesity_df,
    exposure = exposure_df,
    n = 3
  )

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 12 × 6
#>       id sample_id substance_id route_id C_ext  C_ss
#>    <dbl>     <int>        <int>    <int> <dbl> <dbl>
#>  1     1         1            1        1  8.85     1
#>  2     2         1            2        1 20.8     21
#>  3     3         2            1        1  8.69     1
#>  4     4         2            2        1 20.6     21
#>  5     5         3            1        1 10.0      1
#>  6     6         3            2        1 19.9     21
#>  7     7         4            1        1 30.8      2
#>  8     8         4            2        1 39.7     22
#>  9     9         5            1        1 30.4      2
#> 10    10         5            2        1 40.6     22
#> 11    11         6            1        1 29.8     12
#> 12    12         6            2        1 39.7     32

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 6 × 4
#>      id location_id   age weight
#>   <int>       <int> <int> <chr> 
#> 1     1           1    41 Normal
#> 2     2           1    40 Normal
#> 3     3           1    44 Normal
#> 4     4           2    55 Normal
#> 5     5           2    59 Normal
#> 6     6           2    52 Obese 

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

dplyr::tbl(con, "substance") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id casn   
#>   <int> <chr>  
#> 1     1 00-00-1
#> 2     2 00-00-2

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 1 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation

# Note: the 'age', 'weight', 'params', and 'other' columns of the
# 'fixed_css' table contain the C_ss values for sensitivity analysis.
# For example, the 'age' column doesn't contain ages, but C_ss values.
dplyr::tbl(con, "fixed_css") |> dplyr::collect()
#> # A tibble: 12 × 7
#>       id sample_id substance_id   age weight params other
#>    <int>     <int>        <int> <dbl>  <dbl>  <dbl> <dbl>
#>  1     1         1            1     6    1.5      1    11
#>  2     2         1            2    26   21.5     21    11
#>  3     3         2            1     6    1.5      1    11
#>  4     4         2            2    26   21.5     21    11
#>  5     5         3            1     6    1.5      1    11
#>  6     6         3            2    26   21.5     21    11
#>  7     7         4            1     7    1.5      2    17
#>  8     8         4            2    27   21.5     22    17
#>  9     9         5            1     7    1.5      2    17
#> 10    10         5            2    27   21.5     22    17
#> 11    11         6            1     7   11.5      2    17
#> 12    12         6            2    27   31.5     22    17

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
