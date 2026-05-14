# Simulate exposure rates

Simulate exposure rate values for each sample and exposure route based
on parameters in the `exposure_rate_params` table.

## Usage

``` r
simulate_exposure_rate(
  GT,
  rate_extra_cols = NULL,
  overwrite = FALSE,
  sensitivity = FALSE
)
```

## Arguments

- GT:

  GeoTox object.

- rate_extra_cols:

  Additional columns to match from the 'exposure_rate_params' table
  (default NULL).

- overwrite:

  Logical indicating whether to overwrite existing 'exposure_rate' table
  (default FALSE).

- sensitivity:

  Logical indicating whether to simulate exposure rates for sensitivity
  analysis (default FALSE).

## Value

The updated GeoTox object, invisibly.

## Details

An 'exposure_rate_params' table must already exist in the GeoTox
database, which is added using
[`add_exposure_rate_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure_rate_params.md).
There must also be a 'sample' table with an 'age' column, which can be
created using
[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md)
or
[`set_sample()`](https://github.com/NIEHS/GeoTox/dev/reference/set_sample.md).
'location' and 'route' tables must also exist, but they are created when
adding the rate parameters and samples.

If `rate_exta_cols` is provided it will be added to the GeoTox object
parameter list, `GT$par`. These columns must exist in the
'exposure_rate_params' table and will be used to match between the
'sample' and 'exposure_rate_params' tables when simulating exposure
rates.

Typically this function will be called with `sensitivity` set to
`FALSE`. In this case, the function will create an 'exposure_rate' table
with simulated exposure rates for each sample and exposure route.

If `sensitivity` is `TRUE`, exposure rates will be simulated for
sensitivity analysis. Typically this shouldn't be used directly by the
user, but rather called by
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md).
In this case, the existing 'exposure_rate' table will be copied to the
'exposure_rate_sensitivity' table where the rate values will be
overwritten.

## See also

[`add_exposure_rate_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure_rate_params.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

## Examples

``` r
# Setup required tables
# Note: 'gender' is ignored when using the default rate params
sample_df <- tibble::tribble(
  ~FIPS, ~age, ~weight, ~gender,
  10000, 25, "Normal",   "male",
  10000, 35,  "Obese",   "male",
  20000, 50, "Normal", "female"
)
GT <- GeoTox() |>
  add_exposure_rate_params() |>
  set_sample(sample_df)

# Simulate exposure rates
GT |> simulate_exposure_rate()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#> # A tibble: 3 × 3
#>   sample_id route_id  rate
#>       <int>    <int> <dbl>
#> 1         1        1 0.219
#> 2         2        1 0.234
#> 3         3        1 0.265

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 3 × 5
#>      id location_id   age weight gender
#>   <int>       <int> <dbl> <chr>  <chr> 
#> 1     1           1    25 Normal male  
#> 2     2           1    35 Obese  male  
#> 3     3           2    50 Normal female

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 1 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation

# Replace exposure rate params with a new table that includes gender
params_df <- tibble::tribble(
  ~age_lb, ~age_ub, ~gender, ~mean, ~sd,
   0, 49,   "male", 10, 1,
  50, 99,   "male", 20, 1,
   0, 49, "female", 30, 1,
  50, 99, "female", 40, 1
)
DBI::dbRemoveTable(con, "exposure_rate_params")
GT |> add_exposure_rate_params(params = params_df)

# Overwrite 'rate' values in existing 'exposure_rate' table
# Must specify additional column names
# Notice how the column names are added to GT$par
str(GT$par)
#> List of 1
#>  $ reset_seed: logi FALSE
GT <- GT |>
  simulate_exposure_rate(rate_extra_cols = c("gender"), overwrite = TRUE)
str(GT$par)
#> List of 2
#>  $ reset_seed     : logi FALSE
#>  $ rate_extra_cols: chr "gender"

# Look at updated 'exposure_rate' table
dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#> # A tibble: 3 × 3
#>   sample_id route_id  rate
#>       <int>    <int> <dbl>
#> 1         1        1  9.18
#> 2         2        1  9.20
#> 3         3        1 39.0 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
