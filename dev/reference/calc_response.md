# Calculate concentrations and risks

Calculate internal dose, in vitro concentration, and risk estimates.

## Usage

``` r
calc_response(GT, overwrite = FALSE, ...)
```

## Arguments

- GT:

  GeoTox object.

- overwrite:

  Logical indicating whether to overwrite existing values (default
  FALSE).

- ...:

  Additional arguments passed to
  [`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md).

## Value

The same GeoTox object, invisibly.

## Details

This is a wrapper around several other functions:

- [`calc_internal_dose()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_internal_dose.md)
  to calculate internal dose (D_int).

- [`calc_invitro_concentration()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_invitro_concentration.md)
  to calculate in vitro concentration (C_invitro).

- [`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md)
  to calculate risk estimates.

If a `risk_name` argument is provided to `...` and it is not "risk",
then sensitivity analysis is assumed and the `sensitivity` argument in
the internal dose and in vitro concentration calculations is set to
TRUE.

## See also

[`calc_internal_dose()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_internal_dose.md),
[`calc_invitro_concentration()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_invitro_concentration.md),
[`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md)

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
hill_df <- tibble::tribble(
  ~assay, ~casn, ~logc, ~resp,
  "a1", "00-00-1",    0,  10,
  "a1", "00-00-1",    1,  20,
  "a1", "00-00-1",    2,  80,
  "a1", "00-00-1",    3, 100,
  "a1", "00-00-2", -0.5,   5,
  "a1", "00-00-2",  0.5,  20,
  "a1", "00-00-2",  1.5,  55,
  "a1", "00-00-2",  2.5,  60
)
GT <- GeoTox() |>
  set_sample(sample_df) |>
  set_simulated_css(css_df) |>
  add_exposure_rate_params() |>
  add_hill_params(fit_hill(hill_df, assay = "assay", substance = "casn")) |>
  simulate_population(exposure = exposure_df)

# Calculate concentrations and risk
GT <- GT |> calc_response()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant table

dplyr::tbl(con, "concentration") |> dplyr::collect()
#> # A tibble: 6 × 8
#>      id sample_id substance_id route_id C_ext  C_ss D_int C_invitro
#>   <dbl>     <int>        <int>    <int> <dbl> <dbl> <dbl>     <dbl>
#> 1     1         1            1        1 10.5     21  2.59      54.4
#> 2     2         1            2        1 19.7     11  4.88      53.7
#> 3     3         2            1        1  8.67    61  2.23     136. 
#> 4     4         2            2        1 20.7     31  5.32     165. 
#> 5     5         3            1        1 28.6     22  6.98     154. 
#> 6     6         3            2        1 39.5     12  9.65     116. 

dplyr::tbl(con, "risk") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    65.7   71.9      116.     116.
#> 2        1         2    67.5   70.5      311.     311.
#> 3        1         3    67.2   72.8      218.     218.

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
