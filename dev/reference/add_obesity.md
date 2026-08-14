# Add obesity simulation data

Create or add to the 'obesity' table in a GeoTox database.

## Usage

``` r
add_obesity(GT, df, location = "FIPS")
```

## Arguments

- GT:

  GeoTox object.

- df:

  Data frame with obesity simulation data.

- location:

  Column name(s) in `df` that contain location identifier(s) (default
  "FIPS").

## Value

The same GeoTox object, invisibly.

## Details

The simulation data must contain columns for obesity prevalence and
standard deviation (default "OBESITY_CrudePrev" and "OBESITY_SD",
respectfully) and at least one column containing location information
(default "FIPS"). The obesity data is used by
[`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md)
to generate weight category samples for each location.

The `location` input can be a named vector to specify multiple
identifier columns in `df`. For example,
`location = c(FIPS = "FIPS", state = "ST")` would indicate that `df`
contains both FIPS codes and state identifiers for locations. The
`state = "ST"` part would rename the "ST" column in `df` to "state" in
the 'location' table.

## See also

[`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md)

## Examples

``` r
# Example obesity simulation data
obesity_df <- data.frame(
  FIPS = c(10000, 20000),
  OBESITY_CrudePrev = c(20, 80),
  OBESITY_SD = 5
)

# Add obesity simulation data to GeoTox database
GT <- GeoTox() |> add_obesity(obesity_df)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "obesity") |> dplyr::collect()
#> # A tibble: 2 × 4
#>      id location_id OBESITY_CrudePrev OBESITY_SD
#>   <int>       <int>             <dbl>      <dbl>
#> 1     1           1                20          5
#> 2     2           2                80          5

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

# Add another location with additional information
obesity_df <- data.frame(
 FIPS = 30000,
 ST = "State3",
 OBESITY_CrudePrev = 50,
 OBESITY_SD = 10
)
GT |> add_obesity(obesity_df, location = c(FIPS = "FIPS", state = "ST"))

# Look at updated tables

dplyr::tbl(con, "obesity") |> dplyr::collect()
#> # A tibble: 3 × 4
#>      id location_id OBESITY_CrudePrev OBESITY_SD
#>   <int>       <int>             <dbl>      <dbl>
#> 1     1           1                20          5
#> 2     2           2                80          5
#> 3     3           3                50         10

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 3 × 3
#>      id  FIPS state 
#>   <int> <dbl> <chr> 
#> 1     1 10000 NA    
#> 2     2 20000 NA    
#> 3     3 30000 State3

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
