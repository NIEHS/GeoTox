# Set sample data

Create the 'sample' table in the GeoTox database.

## Usage

``` r
set_sample(GT, df, location = "FIPS", overwrite = FALSE)
```

## Arguments

- GT:

  GeoTox object.

- df:

  Data frame containing sample data.

- location:

  Column name(s) in `df` that contain location identifier(s) (default
  "FIPS").

- overwrite:

  Logical indicating whether to overwrite existing 'sample' table and
  remove existing 'concentration' and 'risk' tables (default FALSE).

## Value

The same GeoTox object, invisibly.

## Details

The 'sample' table consists of individual characteristics and location
information. At a minimum, it must contain columns with information on
age, weight category, and location identifier(s).

When the `df` input contains information on age or weight category, the
column names must be "age" and "weight", respectively. The `location`
input (default "FIPS") can be a named vector to specify multiple
identifier columns in `df`. For example,
`location = c(FIPS = "FIPS", state = "ST")` would indicate that `df`
contains both FIPS codes and state identifiers for locations. The
location column(s) will be added to the 'location' table and replaced
with a corresponding "location_id" column in the 'sample' table. The
`state = "ST"` part in the example above would rename the "ST" column in
`df` to "state" in the 'location' table.

There are several other functions that can be used to create or add to
the 'sample' table:
[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md)
to generate age data,
[`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md)
to generate weight category data, and
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md),
which is a wrapper function that can generate both age and weight
category data along with other fields. `set_sample()` can be used in
combination with these functions to first set some known sample data,
e.g. age, and then simulate any missing fields, e.g. weight category.

If `overwrite = TRUE`, any existing 'concentration' and 'risk' tables
will be dropped before creating the 'sample' table. This is because the
existing concentration and risk data would no longer be valid for the
new sample data. Further downstream tables are not dropped
automatically, but can be updated during subsequent simulation and
analysis steps.

## See also

[`simulate_age()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_age.md),
[`simulate_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_obesity.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

## Examples

``` r
# Example sample data
sample_df <- tibble::tribble(
  ~FIPS, ~age, ~weight,
  10000, 25, "Normal",
  10000, 35,  "Obese",
  20000, 50, "Normal"
)

# Set sample data
GT <- GeoTox() |> set_sample(sample_df)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 3 × 4
#>      id location_id   age weight
#>   <int>       <int> <dbl> <chr> 
#> 1     1           1    25 Normal
#> 2     2           1    35 Obese 
#> 3     3           2    50 Normal

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
