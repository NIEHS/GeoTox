# Add age simulation data

Create or add to the 'age' table in a GeoTox database.

## Usage

``` r
add_age(GT, df, location = "FIPS")
```

## Arguments

- GT:

  GeoTox object.

- df:

  Data frame with age simulation data.

- location:

  Column name(s) in `df` that contain location identifier(s) (default
  "FIPS").

## Value

The same GeoTox object, invisibly.

## Details

The simulation data must have columns "AGEGRP" and "TOT_POP" and at
least one column containing location information (default "FIPS"). Each
location must have 19 rows for AGEGRP 0-18, where 1-18 are age groups in
increments of 5, e.g. AGEGRP = 5 would be ages 20 to 24, and AGEGRP = 0
is the combination of all age groups. The age data is used by
[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md)
to generate age samples for each location.

The `location` input can be a named vector to specify multiple
identifier columns in `df`. For example,
`location = c(FIPS = "FIPS", state = "ST")` would indicate that `df`
contains both FIPS codes and state identifiers for locations. The
`state = "ST"` part would rename the "ST" column in `df` to "state" in
the 'location' table.

## See also

[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md)

## Examples

``` r
# Example age simulation data
age_df <- data.frame(
  FIPS = rep(c(10000, 20000), each = 19),
  AGEGRP = rep(0:18, times = 2),
  TOT_POP = 0
)
# FIPS 10000, populate age group 40-44
age_df$TOT_POP[c(1, 10)] = 100
# FIPS 20000, populate age groups 50-59
age_df$TOT_POP[c(1, 12, 13) + 19] = c(200, 100, 100)

# Add age simulation data to GeoTox database
GT <- GeoTox() |> add_age(age_df)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "age") |> dplyr::filter(TOT_POP > 0) |> dplyr::collect()
#> # A tibble: 5 × 4
#>      id location_id AGEGRP TOT_POP
#>   <int>       <int>  <int>   <dbl>
#> 1     1           1      0     100
#> 2    10           1      9     100
#> 3    20           2      0     200
#> 4    31           2     11     100
#> 5    32           2     12     100

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
