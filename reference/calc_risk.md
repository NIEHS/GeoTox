# Calculate risk scores

Calculate generalized concentration addition (GCA) and independent
action (IA) risk scores and hazard quotients (HQ) based on in vitro
concentration data and Hill parameters.

## Usage

``` r
calc_risk(
  GT,
  max_mult = 1.5,
  fixed = FALSE,
  overwrite = FALSE,
  risk_name = "risk"
)
```

## Arguments

- GT:

  GeoTox object.

- max_mult:

  Upper bound multiplier for max response (default 1.5).

- fixed:

  Logical indicating whether to set standard deviation parameters of
  Hill fit to zero (default FALSE). Used in sensitivity analysis.

- overwrite:

  Logical indicating whether to overwrite existing risk table (default
  FALSE).

- risk_name:

  Table name to store risk results (default "risk"). Values other than
  "risk" are used in sensitivity analysis.

## Value

The same GeoTox object, invisibly.

## Details

This function requires that the 'concentration', 'sample', and
'hill_params' tables are present in the GeoTox object. Typically these
tables will have been created by prior calls to
[`calc_invitro_concentration()`](https://github.com/NIEHS/GeoTox/reference/calc_invitro_concentration.md)
and
[`add_hill_params()`](https://github.com/NIEHS/GeoTox/reference/add_hill_params.md).

The risk scores are calculated for each sample and assay combination and
stored in the 'risk' table in the GeoTox object, unless a different
`risk_name` is provided. Supplying a different `risk_name` shouldn't be
done directly by the user, but rather by calling
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md).

## See also

[`add_hill_params()`](https://github.com/NIEHS/GeoTox/reference/add_hill_params.md),
[`calc_invitro_concentration()`](https://github.com/NIEHS/GeoTox/reference/calc_invitro_concentration.md),
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
  calc_internal_dose() |>
  calc_invitro_concentration()

# Calculate risk
GT <- GT |> calc_risk()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant table
dplyr::tbl(con, "risk") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    63.5   68.9      90.7     90.7
#> 2        1         2    64.2   67.9     209.     209. 
#> 3        1         3    68.9   75.9     215.     215. 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
