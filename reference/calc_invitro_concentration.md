# Calculate in vitro concentration

Calculates the in vitro concentration (C_invitro) as the product of
steady state plasma concentration (C_ss) and internal dose (D_int), and
stores the results in the 'C_invitro' column of the 'concentration'
table in the GeoTox database.

## Usage

``` r
calc_invitro_concentration(GT, overwrite = FALSE, sensitivity = FALSE)
```

## Arguments

- GT:

  GeoTox object.

- overwrite:

  Logical indicating whether to overwrite existing 'C_invitro' values in
  the 'concentration' table (default FALSE).

- sensitivity:

  Logical indicating whether to simulate in vitro concentration for
  sensitivity analysis (default FALSE).

## Value

The same GeoTox object, invisibly.

## Details

If `sensitivity = TRUE`, 'C_invitro' will be calculated for sensitivity
analysis. Typically this shouldn't be used directly by the user, but
rather called by
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md).
In this case, the function will use the 'concentration_sensitivity'
table instead of the 'concentration' table.

## See also

[`calc_internal_dose()`](https://github.com/NIEHS/GeoTox/reference/calc_internal_dose.md),
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
css_df <- tibble::tribble(
  ~casn, ~age_lb, ~age_ub, ~weight, ~css,
  "00-00-1",  0, 49, "Normal", 21,
  "00-00-1", 50, 99, "Normal", 22,
  "00-00-1",  0, 49,  "Obese", 61,
  "00-00-1", 50, 99,  "Obese", 62,
  "00-00-2",  0, 49, "Normal", 11,
  "00-00-2", 50, 99, "Normal", 12,
  "00-00-2",  0, 49,  "Obese", 31,
  "00-00-2", 50, 99,  "Obese", 32
)
GT <- GeoTox() |>
  set_sample(sample_df) |>
  set_simulated_css(css_df) |>
  add_exposure_rate_params() |>
  simulate_population(exposure = exposure_df) |>
  calc_internal_dose()

# Calculate in vitro concentration
GT <- GT |> calc_invitro_concentration()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant tables
dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 8
#>      id sample_id substance_id route_id C_ext  C_ss D_int C_invitro
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl> <dbl>     <dbl>
#> 1     1         1            1        1  9.24    21  2.40      50.3
#> 2     2         1            2        1 20.8     11  5.38      59.2
#> 3     3         2            1        1  8.77    61  2.60     158. 
#> 4     4         2            2        1 19.4     31  5.73     178. 
#> 5     5         3            1        1 29.3     22  7.67     169. 
#> 6     6         3            2        1 41.6     12 10.9      131. 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
