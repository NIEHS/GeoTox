# Get concentration mean values

Calculates the concentration mean values for each substance and route at
each location based on the data in the 'concentration' and 'sample'
tables of the GeoTox database. The output of this function is useful for
plotting or further analysis.

## Usage

``` r
get_concentration_mean(GT, col)
```

## Arguments

- GT:

  GeoTox object.

- col:

  Column name in the 'concentration' table for which to calculate the
  mean, grouped by substance, route, and location.

## Value

A data frame.

## Details

The `col` parameter specifies which column in the 'concentration' table
to use for the mean calculation. The available choices will depend on
what data has been stored in the 'concentration' table during the
simulation process.

## Examples

``` r
# Setup required tables
exposure_df <- tibble::tribble(
  ~FIPS, ~casn, ~route, ~mean, ~sd,
  10000, "00-00-1", "inhalation", 10, 1,
  10000, "00-00-2", "inhalation", 20, 1,
  20000, "00-00-1", "inhalation", 30, 1,
  20000, "00-00-2", "inhalation", 40, 1
)
GT <- GeoTox() |>
  add_exposure(exposure_df) |>
  simulate_exposure(n = 100)

# Calculate mean external concentration by substance and location
get_concentration_mean(GT, "C_ext")
#> # A tibble: 4 × 4
#>   substance_id route_id location_id  mean
#>          <int>    <int>       <int> <dbl>
#> 1            1        1           1  9.81
#> 2            1        1           2 30.1 
#> 3            2        1           1 19.9 
#> 4            2        1           2 40.0 

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at column names in the 'concentration' table
dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 400 × 5
#>       id sample_id substance_id route_id C_ext
#>    <dbl>     <int>        <int>    <int> <dbl>
#>  1     1         1            1        1 10.0 
#>  2     2         1            2        1 19.5 
#>  3     3         2            1        1 10.7 
#>  4     4         2            2        1 19.1 
#>  5     5         3            1        1  8.73
#>  6     6         3            2        1 18.5 
#>  7     7         4            1        1  9.84
#>  8     8         4            2        1 19.8 
#>  9     9         5            1        1  9.39
#> 10    10         5            2        1 21.6 
#> # ℹ 390 more rows

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
