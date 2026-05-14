# Simulate exposure concentrations

Simulate external exposure (C_ext) values to be stored in the
'concentration' table of a GeoTox database.

## Usage

``` r
simulate_exposure(
  GT,
  n = 1000,
  overwrite = FALSE,
  expos_mean = NULL,
  expos_sd = NULL,
  sensitivity = FALSE
)
```

## Arguments

- GT:

  GeoTox object.

- n:

  Number of individuals to simulate per location (default 1000). Ignored
  if 'sample' table already exists.

- overwrite:

  Logical indicating whether to overwrite existing 'C_ext' values in the
  'concentration' table (default FALSE).

- expos_mean:

  Column name of exposure concentration mean in the 'exposure' table
  (default "mean").

- expos_sd:

  Column name of exposure concentration standard deviation in the
  'exposure' table (default "sd").

- sensitivity:

  Logical indicating whether to simulate exposures for sensitivity
  analysis (default FALSE).

## Value

The updated GeoTox object, invisibly.

## Details

An 'external' table containing simulation data must already exist in the
GeoTox database, which is added using
[`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md).

The inputs `expos_mean` and `expos_sd` will be assigned default values
of "mean" and "sd", respectively, if not provided and not already set in
the GeoTox object's parameters, `GT$par`. If not `NULL`, the provided
values will also be saved to `GT$par`.

If `sensitivity = TRUE`, exposure concentrations will be simulated for
sensitivity analysis. Typically this shouldn't be used directly by the
user, but rather called by
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md).
In this case, the function will use the 'concentration_sensitivity'
table instead of the 'concentration' table, and will assume that the
'sample' table already exists.

## See also

[`add_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/add_exposure.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

## Examples

``` r
# Example exposure simulation data
exposure_df <- tibble::tribble(
  ~FIPS, ~casn, ~route, ~mean, ~sd,
  10000, "00-00-1", "inhalation", 10, 1,
  10000, "00-00-2", "inhalation", 20, 1,
  20000, "00-00-1", "inhalation", 30, 1,
  20000, "00-00-2", "inhalation", 40, 1
)

# Simulate C_ext values
GT <- GeoTox() |>
  add_exposure(exposure_df) |>
  simulate_exposure(n = 3)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 12 × 5
#>       id sample_id substance_id route_id C_ext
#>    <dbl>     <int>        <int>    <int> <dbl>
#>  1     1         1            1        1 11.2 
#>  2     2         1            2        1 21.4 
#>  3     3         2            1        1 10.6 
#>  4     4         2            2        1 20.4 
#>  5     5         3            1        1  9.41
#>  6     6         3            2        1 20.6 
#>  7     7         4            1        1 29.6 
#>  8     8         4            2        1 40.6 
#>  9     9         5            1        1 29.8 
#> 10    10         5            2        1 40.6 
#> 11    11         6            1        1 30.2 
#> 12    12         6            2        1 41.8 

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 6 × 2
#>      id location_id
#>   <int>       <int>
#> 1     1           1
#> 2     2           1
#> 3     3           1
#> 4     4           2
#> 5     5           2
#> 6     6           2

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

# Replace 'exposure' table with different column names
names(exposure_df)[4:5] <- c("mu", "sigma")
DBI::dbRemoveTable(con, "exposure")
GT |> add_exposure(exposure_df)

# Overwrite 'C_ext' values in existing 'concentration' table
# Must specify new 'exposure' column names
# Notice how the column names are added to GT$par
str(GT$par)
#> List of 1
#>  $ reset_seed: logi FALSE
GT <- GT |>
  simulate_exposure(expos_mean = "mu", expos_sd = "sigma", overwrite = TRUE)
str(GT$par)
#> List of 3
#>  $ reset_seed: logi FALSE
#>  $ expos_mean: chr "mu"
#>  $ expos_sd  : chr "sigma"

# Look at updated 'concentration' table
dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 12 × 5
#>       id sample_id substance_id route_id C_ext
#>    <dbl>     <int>        <int>    <int> <dbl>
#>  1     1         1            1        1 10.8 
#>  2     2         1            2        1 20.4 
#>  3     3         2            1        1  9.82
#>  4     4         2            2        1 20.1 
#>  5     5         3            1        1  9.66
#>  6     6         3            2        1 21.1 
#>  7     7         4            1        1 31.7 
#>  8     8         4            2        1 39.9 
#>  9     9         5            1        1 29.1 
#> 10    10         5            2        1 40.4 
#> 11    11         6            1        1 31.0 
#> 12    12         6            2        1 38.9 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
