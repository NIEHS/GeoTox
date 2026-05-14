# Set pre-simulated steady-state plasma concentrations

Create the 'simulated_css' table in a GeoTox database, which contains
pre-simulated steady-state plasma concentrations (C\\\_{ss}\\).

## Usage

``` r
set_simulated_css(GT, df, substance = "casn", overwrite = FALSE)
```

## Arguments

- GT:

  GeoTox object.

- df:

  Data frame containing simulated C\\\_{ss}\\ values for groups of
  population characteristics.

- substance:

  Column name(s) in `df` that contain substance identifier(s) (default
  "casn").

- overwrite:

  Logical indicating whether to overwrite existing 'simulated_css' table
  (default FALSE).

## Value

The same GeoTox object, invisibly.

## Details

The 'simulated_css' table is used by
[`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md)
to assign C\\\_{ss}\\ values to individuals. The minimum required
columns in the `df` data frame are "age_lb", "age_ub", "weight", "css",
and at least one column with substance information (default "casn").

The values for "age_lb" and "age_ub" should be non-overlapping integers
representing age ranges (in years). For example, two subsequent age
groups might be `c(0, 4)` and `c(5, 9)`. The "weight" column should
contain the weight category and contain values of either "Normal" or
"Obese". The "css" column should contain the pre-simulated C\\\_{ss}\\
values.

The `substance` input can be a named vector to specify multiple
substance identifier columns in `df`. For example,
`c(casn = "casn", name = "chnm")` would indicate that `df` contains both
CAS numbers and chemical names for substances. The `name = "chnm"` part
would rename the "chnm" column in `df` to "name" in the 'substance'
table.

## See also

[`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/sample_simulated_css.md)

## Examples

``` r
# Example pre-simulated C_ss data
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

# Set simulated C_ss values
GT <- GeoTox() |> set_simulated_css(css_df)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant tables

dplyr::tbl(con, "simulated_css") |> dplyr::collect()
#> # A tibble: 8 × 6
#>      id substance_id age_lb age_ub weight   css
#>   <int>        <int>  <dbl>  <dbl> <chr>  <dbl>
#> 1     1            1      0     49 Normal     1
#> 2     2            1     50     99 Normal     2
#> 3     3            1      0     49 Obese     11
#> 4     4            1     50     99 Obese     12
#> 5     5            2      0     49 Normal    21
#> 6     6            2     50     99 Normal    22
#> 7     7            2      0     49 Obese     31
#> 8     8            2     50     99 Obese     32

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
