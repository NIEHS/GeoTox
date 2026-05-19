# Calculate internal dose

Calculates internal dose (D_int) as the product of external
concentration ('C_ext' in the 'concentration' table) and exposure rate
('rate' in the 'exposure_rate' table), and stores the results in the
'D_int' column of the 'concentration' table in the GeoTox database.

## Usage

``` r
calc_internal_dose(GT, overwrite = FALSE, sensitivity = FALSE)
```

## Arguments

- GT:

  GeoTox object.

- overwrite:

  Logical indicating whether to overwrite existing 'D_int' values in the
  'concentration' table (default FALSE).

- sensitivity:

  Logical indicating whether to simulate internal dose for sensitivity
  analysis (default FALSE).

## Value

The same GeoTox object, invisibly.

## Details

If `sensitivity = TRUE`, 'D_int' will be calculated for sensitivity
analysis. Typically this shouldn't be used directly by the user, but
rather called by
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md).
In this case, the function will use the 'concentration_sensitivity' and
'exposure_rate_sensitivity' tables instead of the 'concentration' and
'exposure_rate' tables.

## See also

[`calc_response()`](https://github.com/NIEHS/GeoTox/reference/calc_response.md)

## Examples

``` r
# Setup required tables
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
GT <- GeoTox() |>
  set_sample(sample_df) |>
  add_exposure_rate_params() |>
  simulate_population(exposure = exposure_df, sample_css = FALSE)

# Calculate internal dose
GT <- GT |> calc_internal_dose()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant tables

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 6
#>      id sample_id substance_id route_id C_ext D_int
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl>
#> 1     1         1            1        1 10.7   3.06
#> 2     2         1            2        1 20.2   5.78
#> 3     3         2            1        1  8.47  2.37
#> 4     4         2            2        1 18.7   5.22
#> 5     5         3            1        1 31.4   5.87
#> 6     6         3            2        1 40.0   7.48

dplyr::tbl(con, "exposure_rate") |> dplyr::collect()
#> # A tibble: 3 × 3
#>   sample_id route_id  rate
#>       <int>    <int> <dbl>
#> 1         1        1 0.286
#> 2         2        1 0.279
#> 3         3        1 0.187

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
