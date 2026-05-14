# Simulate age values

Simulate 'age' values to be stored in the 'sample' table of a GeoTox
database.

## Usage

``` r
simulate_age(GT, n = 1000, overwrite = FALSE)
```

## Arguments

- GT:

  GeoTox object.

- n:

  Number of individuals to simulate per location (default 1000). Ignored
  if 'sample' table already exists.

- overwrite:

  Logical indicating whether to overwrite existing 'age' values in the
  'sample' table (default FALSE).

## Value

The same GeoTox object, invisibly.

## Details

An 'age' table containing simulation data must already exist in the
GeoTox database, which is added using
[`add_age()`](https://github.com/NIEHS/GeoTox/dev/reference/add_age.md).

## See also

[`add_age()`](https://github.com/NIEHS/GeoTox/dev/reference/add_age.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

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

# Simulate age values
GT <- GeoTox() |>
  add_age(age_df) |>
  simulate_age(n = 5)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 10 × 3
#>       id location_id   age
#>    <int>       <int> <int>
#>  1     1           1    40
#>  2     2           1    41
#>  3     3           1    40
#>  4     4           1    42
#>  5     5           1    40
#>  6     6           2    51
#>  7     7           2    55
#>  8     8           2    55
#>  9     9           2    50
#> 10    10           2    55

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

# Overwrite existing age values
GT <- GT |> simulate_age(overwrite = TRUE)

# Look at updated 'sample' table
dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 10 × 3
#>       id location_id   age
#>    <int>       <int> <int>
#>  1     1           1    40
#>  2     2           1    44
#>  3     3           1    44
#>  4     4           1    41
#>  5     5           1    42
#>  6     6           2    59
#>  7     7           2    54
#>  8     8           2    51
#>  9     9           2    59
#> 10    10           2    56

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
