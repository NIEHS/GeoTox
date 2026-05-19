# Simulate obesity values

Simulate 'weight' category values to be stored in the 'sample' table of
a GeoTox database.

## Usage

``` r
simulate_obesity(
  GT,
  n = 1000,
  overwrite = FALSE,
  obes_prev = NULL,
  obes_sd = NULL
)
```

## Arguments

- GT:

  GeoTox object.

- n:

  Number of individuals to simulate per location (default 1000). Ignored
  if 'sample' table already exists.

- overwrite:

  Logical indicating whether to overwrite existing 'weight' values in
  the 'sample' table (default FALSE).

- obes_prev:

  Column name of obesity prevalence in the 'obesity' table (default
  "OBESITY_CrudePrev").

- obes_sd:

  Column name of obesity standard deviation in the 'obesity' table
  (default "OBESITY_SD").

## Value

The updated GeoTox object, invisibly.

## Details

An 'obesity' table containing simulation data must already exist in the
GeoTox database, which is added using
[`add_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/add_obesity.md).

The inputs `obes_prev` and `obes_sd` will be assigned default values of
"OBESITY_CrudePrev" and "OBESITY_SD", respectively, if not provided and
not already set in the GeoTox object's parameters, `GT$par`. If not
`NULL`, the provided values will also be saved to `GT$par`.

## See also

[`add_obesity()`](https://github.com/NIEHS/GeoTox/dev/reference/add_obesity.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

## Examples

``` r
# Example obesity simulation data
obesity_df <- data.frame(
  FIPS = c(10000, 20000),
  OBESITY_CrudePrev = c(20, 80),
  OBESITY_SD = 5
)

# Simulate weight category values
GT <- GeoTox() |>
  add_obesity(obesity_df) |>
  simulate_obesity(n = 5)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 10 × 3
#>       id location_id weight
#>    <int>       <int> <chr> 
#>  1     1           1 Normal
#>  2     2           1 Normal
#>  3     3           1 Normal
#>  4     4           1 Normal
#>  5     5           1 Obese 
#>  6     6           2 Obese 
#>  7     7           2 Obese 
#>  8     8           2 Obese 
#>  9     9           2 Normal
#> 10    10           2 Obese 

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

# Replace 'obesity' table with different column names
names(obesity_df)[2:3] <- c("prev", "sd")
DBI::dbRemoveTable(con, "obesity")
GT |> add_obesity(obesity_df)

# Overwrite 'weight' category values in existing 'sample' table
# Must specify new 'obesity' column names
# Notice how the column names are added to GT$par
str(GT$par)
#> List of 1
#>  $ reset_seed: logi FALSE
GT <- GT |>
  simulate_obesity(obes_prev = "prev", obes_sd = "sd", overwrite = TRUE)
str(GT$par)
#> List of 3
#>  $ reset_seed: logi FALSE
#>  $ obes_prev : chr "prev"
#>  $ obes_sd   : chr "sd"

# Look at updated 'sample' table
dplyr::tbl(con, "sample") |> dplyr::collect()
#> # A tibble: 10 × 3
#>       id location_id weight
#>    <int>       <int> <chr> 
#>  1     1           1 Normal
#>  2     2           1 Normal
#>  3     3           1 Normal
#>  4     4           1 Normal
#>  5     5           1 Obese 
#>  6     6           2 Obese 
#>  7     7           2 Obese 
#>  8     8           2 Obese 
#>  9     9           2 Obese 
#> 10    10           2 Obese 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
