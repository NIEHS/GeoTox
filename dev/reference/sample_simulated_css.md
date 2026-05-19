# Sample from pre-simulated steady-state plasma concentrations

Sample steady-state plasma concentrations (C\\\_{ss}\\) for individuals
from pre-simulated values stored in the 'simulated_css' table in a
GeoTox database.

## Usage

``` r
sample_simulated_css(GT, css_extra_cols = NULL, substance_order = NULL)
```

## Arguments

- GT:

  GeoTox object.

- css_extra_cols:

  Additional columns to match from the 'simulated_css' table (default
  NULL).

- substance_order:

  Named list specifying order of substance evaluation (default NULL).

## Value

The updated GeoTox object, invisibly.

## Details

The C\\\_{ss}\\ values are sampled from the 'simulated_css' table, which
must already exist in the GeoTox database; it can be created using
[`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md).
The minimum characteristics that are used to match individuals to sets
of C\\\_{ss}\\ values are age and weight category ("Normal" or "Obese").
Additional columns (which must exist in both the 'simulated_css' and
'sample' tables) can be specified using the `css_extra_cols` argument.
If `css_exta_cols` is provided it will be added to the GeoTox object
parameter list, `GT$par`.

In addition to the 'simulated_css' table, both 'sample' and
'concentration' tables must also exist in the GeoTox database. The
'sample' table must contain the characteristics of individuals (age,
weight category, and any additional columns specified in
`css_extra_cols`). One way to create this 'sample' table is by using
[`set_sample()`](https://github.com/NIEHS/GeoTox/dev/reference/set_sample.md).
The 'concentration' table will typically be created using
[`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md)
and will be populated with rows for each individual and substance
combination where exposure data is available. The sampled C\\\_{ss}\\
values will be stored in a new "C_ss" column in the 'concentration'
table.

The `substance_order` argument can be used to specify the order in which
substances are evaluated when sampling C\\\_{ss}\\ values. The default
is to evaluate substances in the order they appear in the 'substance'
table. However, if a different order is needed for some reason (e.g.,
replication of results from a previous GeoTox implementation), the user
can provide a named list where the name is the column in the 'substance'
table to use for ordering (e.g., "casn") and the value is a vector of
substance identifiers in the desired order. If provided, the
`substance_order` will be added to the GeoTox object parameter list,
`GT$par`.

## See also

[`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md),
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)

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
  set_simulated_css(css_df)

# Sample simulated C_ss values
GT <- GT |> sample_simulated_css()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables. sample_simulated_css() generated the 'C_ss' column
# of the 'concentration' table.

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 6
#>      id sample_id substance_id route_id C_ext  C_ss
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl>
#> 1     1         1            1        1  8.54     1
#> 2     2         1            2        1 20.5     21
#> 3     3         2            1        1  9.20    11
#> 4     4         2            2        1 18.8     31
#> 5     5         3            1        1 30.4      2
#> 6     6         3            2        1 40.9     22

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

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 1 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation

# Replace sample and css tables with new data including an extra column
# Limit to a single substance for simplicity
sample_df <- tibble::tribble(
  ~FIPS, ~age, ~weight, ~sign,
  10000, 25, "Normal", "+",
  10000, 35,  "Obese", "-",
  20000, 50, "Normal", "+"
)
css_df <- tibble::tribble(
  ~casn, ~age_lb, ~age_ub, ~weight, ~css, ~sign,
  "00-00-1",  0, 49, "Normal",   1, "+",
  "00-00-1", 50, 99, "Normal",   2, "+",
  "00-00-1",  0, 49,  "Obese",  11, "+",
  "00-00-1", 50, 99,  "Obese",  12, "+",
  "00-00-1",  0, 49, "Normal",  -1, "-",
  "00-00-1", 50, 99, "Normal",  -2, "-",
  "00-00-1",  0, 49,  "Obese", -11, "-",
  "00-00-1", 50, 99,  "Obese", -12, "-"
)
GT <- GT |>
 set_sample(sample_df, overwrite = TRUE) |>
 simulate_exposure() |>
 set_simulated_css(css_df, overwrite = TRUE)

# Sample simulated C_ss values with extra column
# Notice how the extra column name is added to GT$par
str(GT$par)
#> List of 1
#>  $ reset_seed: logi FALSE
GT <- GT |> sample_simulated_css(css_extra_cols = "sign")
str(GT$par)
#> List of 2
#>  $ reset_seed    : logi FALSE
#>  $ css_extra_cols: chr "sign"

# Look at new 'concentration' table. Values will be missing for substance 2
# since it is not in the new css_df.
dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 6
#>      id sample_id substance_id route_id C_ext  C_ss
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl>
#> 1     1         1            1        1  8.75     1
#> 2     2         1            2        1 19.5     NA
#> 3     3         2            1        1 10.2    -11
#> 4     4         2            2        1 21.4     NA
#> 5     5         3            1        1 30.3      2
#> 6     6         3            2        1 40.4     NA

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
