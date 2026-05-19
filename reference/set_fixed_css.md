# Prepare steady-state plasma concentrations for sensitivity analysis

Create the 'fixed_css' table in the GeoTox database, which contains
values of steady-state plasma concentrations (C\\\_{ss}\\) for
sensitivity analysis.

## Usage

``` r
set_fixed_css(GT, substance_order = NULL)
```

## Arguments

- GT:

  GeoTox object.

- substance_order:

  Named list specifying order of substance evaluation (default NULL).

## Value

The updated GeoTox object, invisibly.

## Details

Several tables are required in the GeoTox database before the
'fixed_css' table can be created. Typically, `set_fixed_css()` is called
after using
[`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/reference/sample_simulated_css.md),
at which point all required tables will be present.

The resulting 'fixed_css' table is used for sensitivity analysis where
one parameter is allowed to vary at a time while all other parameters
are held constant at fixed values.

The `substance_order` argument can be used to specify the order in which
substances are evaluated when sampling C\\\_{ss}\\ values. The default
is to evaluate substances in the order they appear in the 'substance'
table. However, if a different order is needed for some reason (e.g.,
replication of results from a previous GeoTox implementation), the user
can provide a named list where the name is the column in the 'substance'
table to use for ordering (e.g., "casn") and the value is a vector of
substance identifiers in the desired order. If provided, the
`substance_order` will be added to the GeoTox object parameter list,
`GT$par`. This is only applicable to the "params" section described
below.

### Table 'fixed_css' columns:

- age:

  Median pre-simulated C\\\_{ss}\\ values are computed by age group for
  each substance, then the median C\\\_{ss}\\ values are assigned to
  each individual based on their age group.

- weight:

  Median pre-simulated C\\\_{ss}\\ values are computed by weight
  category for each substance, then the median C\\\_{ss}\\ values are
  assigned to each individual based on their weight category.

- params:

  Individuals are assigned the median age of their location and
  pre-simulated C\\\_{ss}\\ values for the "Normal" weight category are
  sampled.

- other:

  Median sampled C\\\_{ss}\\ values are computed across all substances
  for each location after mean-imputation of missing C\\\_{ss}\\ values
  for each substance and location. The median C\\\_{ss}\\ values are
  assigned to each individual based on their location.

## See also

[`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/reference/sample_simulated_css.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/reference/simulate_population.md)

## Examples

``` r
# Create required tables
sample_df <- tibble::tribble(
  ~FIPS, ~age, ~weight,
  10000, 25, "Normal",
  10000, 35,  "Obese",
  20000, 50, "Normal"
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
GT <- GeoTox() |>
  set_sample(sample_df) |>
  add_exposure(exposure_df) |>
  simulate_exposure() |>
  set_simulated_css(css_df) |>
  sample_simulated_css()

# Set fixed C_ss values
GT <- GT |> set_fixed_css()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables
# Note: the 'age', 'weight', 'params', and 'other' columns of the
# 'fixed_css' table contain the C_ss values for sensitivity analysis.
# For example, the 'age' column doesn't contain ages, but C_ss values.

dplyr::tbl(con, "fixed_css") |> dplyr::collect()
#> # A tibble: 6 × 7
#>      id sample_id substance_id   age weight params other
#>   <int>     <int>        <int> <dbl>  <dbl>  <dbl> <dbl>
#> 1     1         1            1     6    1.5      1    16
#> 2     2         1            2    26   21.5     21    16
#> 3     3         2            1     6   11.5      1    16
#> 4     4         2            2    26   31.5     21    16
#> 5     5         3            1     7    1.5      2    12
#> 6     6         3            2    27   21.5     22    12

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 6
#>      id sample_id substance_id route_id C_ext  C_ss
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl>
#> 1     1         1            1        1  9.60     1
#> 2     2         1            2        1 19.1     21
#> 3     3         2            1        1  6.74    11
#> 4     4         2            2        1 21.3     31
#> 5     5         3            1        1 30.9      2
#> 6     6         3            2        1 38.4     22

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

dplyr::tbl(con, "substance") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id casn   
#>   <int> <chr>  
#> 1     1 00-00-1
#> 2     2 00-00-2

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
