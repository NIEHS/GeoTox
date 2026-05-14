# Add exposure simulation data

Create or add to the 'exposure' table in a GeoTox database.

## Usage

``` r
add_exposure(GT, df, location = "FIPS", substance = "casn", route = "route")
```

## Arguments

- GT:

  GeoTox object.

- df:

  Data frame with exposure simulation data.

- location:

  Column name(s) in `df` that contain location identifier(s) (default
  "FIPS").

- substance:

  Column name(s) in `df` that contain substance identifier(s) (default
  "casn").

- route:

  Column name in `df` that contains route identifier (default "route").

## Value

The same GeoTox object, invisibly.

## Details

The simulation data must contain columns for exposure mean and standard
deviation (default "mean" and "sd", respectfully), at least one column
containing location information (default "FIPS"), at least one column
containing substance information (default "casn"), and one column
containing route information (default "route"). The exposure data is
used by
[`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md)
to generate external exposure concentration samples for each location.

The `location` and `substance` inputs can be named vectors to specify
multiple identifier columns in `df`. For example,
`substance = c(casn = "casn", name = "chnm")` would indicate that `df`
contains both CAS numbers and chemical names for substances. The
`name = "chnm"` part would rename the "chnm" column in `df` to "name" in
the 'substance' table.

## See also

[`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md)

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

# Add exposure simulation data to GeoTox database
GT <- GeoTox() |> add_exposure(exposure_df)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables
dplyr::tbl(con, "exposure") |> dplyr::collect()
#> # A tibble: 4 × 6
#>      id location_id substance_id route_id  mean    sd
#>   <int>       <int>        <int>    <int> <dbl> <dbl>
#> 1     1           1            1        1    10     1
#> 2     2           1            2        1    20     1
#> 3     3           2            1        1    30     1
#> 4     4           2            2        1    40     1
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

# Add another substance with a new field and route
exposure_df <- tibble::tribble(
  ~FIPS, ~casn, ~chnm, ~route, ~mean, ~sd,
  10000, "00-00-3", "chem3", "drinking", 100, 1,
  20000, "00-00-3", "chem3", "drinking", 200, 1
)
GT |> add_exposure(exposure_df, substance = c(casn = "casn", name = "chnm"))

# Look at updated tables

dplyr::tbl(con, "exposure") |> dplyr::collect()
#> # A tibble: 6 × 6
#>      id location_id substance_id route_id  mean    sd
#>   <int>       <int>        <int>    <int> <dbl> <dbl>
#> 1     1           1            1        1    10     1
#> 2     2           1            2        1    20     1
#> 3     3           2            1        1    30     1
#> 4     4           2            2        1    40     1
#> 5     5           1            3        2   100     1
#> 6     6           2            3        2   200     1

dplyr::tbl(con, "substance") |> dplyr::collect()
#> # A tibble: 3 × 3
#>      id casn    name 
#>   <int> <chr>   <chr>
#> 1     1 00-00-1 NA   
#> 2     2 00-00-2 NA   
#> 3     3 00-00-3 chem3

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation
#> 2     2 drinking  

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
