# Calculate risk sensitivity to a single variable

Compute risk by varying one variable while holding others fixed.

## Usage

``` r
calc_sensitivity(
  GT,
  vary = c("age", "weight", "css_params", "fit_params", "C_ext"),
  overwrite = FALSE,
  rate_extra_cols = NULL,
  expos_mean = NULL,
  expos_sd = NULL,
  max_mult = 1.5
)
```

## Arguments

- GT:

  GeoTox object.

- vary:

  Variable to vary. One of "age", "weight", "css_params", "fit_params",
  or "C_ext".

- overwrite:

  Logical indicating whether to overwrite existing sensitivity analysis
  results in the GeoTox database.

- rate_extra_cols:

  Additional columns to match from the 'exposure_rate_params' table
  (default NULL).

- expos_mean:

  Column name of exposure concentration mean in the 'exposure' table
  (default "mean").

- expos_sd:

  Column name of exposure concentration standard deviation in the
  'exposure' table (default "sd").

- max_mult:

  Upper bound multiplier for max response (default 1.5).

## Value

The updated GeoTox object, invisibly.

## Details

The sensitivity analysis makes use of the C\\\_{ss}\\ values stored in
the 'fixed_css' table of the GeoTox database. These values are
determined using the pre-simulated C\\\_{ss}\\ values supplied to
[`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md)
and can be set using
[`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md)
prior to running this function. This step is automatically done when
using
[`simulate_population()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_population.md)
with `sample_css = TRUE`.

There are five options for the `vary` argument:

- age:

  C\\\_{ss}\\ values from the 'age' column of the 'fixed_css' table are
  used. For other cases, exposure rates are re-simulated using the
  median age by location in the 'sample' table by calling
  [`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md)
  with `sensitivity = TRUE`.

- weight:

  C\\\_{ss}\\ values from the 'weight' column of the 'fixed_css' table
  are used.

- css_params:

  C\\\_{ss}\\ values from the 'params' column of the 'fixed_css' table
  are used.

- fit_params:

  C\\\_{ss}\\ values from the 'other' column of the 'fixed_css' table
  are used. For other cases, the standard deviation of dose-response
  model fit parameters are set to zero by calling
  [`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md)
  with `fixed = TRUE`.

- C_ext:

  C\\\_{ss}\\ values from the 'other' column of the 'fixed_css' table
  are used. For other cases, external concentrations are re-simulated
  with standard deviations set to zero by calling
  [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure.md)
  with `sensitivity = TRUE`.

In all cases above, the resulting risk table is named
'risk_sensitivity\_~vary~' (e.g., 'risk_sensitivity_age') in the GeoTox
database.

Inputs `rate_extra_cols`, `expos_mean`, and `expos_sd` do not need to be
specified again if they were already provided in a previous call and are
set in the GeoTox parameters (`GT$par`).

## See also

[`set_simulated_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_simulated_css.md),
[`set_fixed_css()`](https://github.com/NIEHS/GeoTox/dev/reference/set_fixed_css.md),
[`sensitivity_analysis()`](https://github.com/NIEHS/GeoTox/dev/reference/sensitivity_analysis.md)

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
  simulate_population(exposure = exposure_df) |>
  calc_response()

# Calculate sensitivity to age
GT <- GT |> calc_sensitivity("age")

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant table

dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    63.6   68.5      119.     119.
#> 2        1         2    63.8   68.7      124.     124.
#> 3        1         3    68.7   73.8      296.     296.

# Compared to baseline risk table
dplyr::tbl(con, "risk") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.8   67.0      70.0     70.0
#> 2        1         2    66.6   72.3     135.     135. 
#> 3        1         3    64.7   70.6     174.     174. 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
