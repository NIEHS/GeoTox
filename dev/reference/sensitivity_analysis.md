# Perform sensitivity analysis

Calculate risk sensitivity to all available `vary` parameters in
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md).

## Usage

``` r
sensitivity_analysis(GT, max_mult = c(1.5, 1.5, 1.5, 1.5, 1.5), ...)
```

## Arguments

- GT:

  GeoTox object.

- max_mult:

  Vector of length 5 containing upper bound multipliers for max response
  (default 1.5).

- ...:

  Additional arguments passed to each call of
  [`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md).

## Value

The updated GeoTox object, invisibly.

## Details

Sensitivity is calculated in the order: age, weight, css_params,
fit_params, C_ext. The `max_mult` vector allows specifying different
upper bound multipliers for each parameter.

## See also

[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md)

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

# Perform sensitivity analysis
GT <- GT |> sensitivity_analysis()

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant table

dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    65.1   69.7      177.     177.
#> 2        1         2    64.9   69.6      166.     166.
#> 3        1         3    68.9   73.9      336.     336.

dplyr::tbl(con, "risk_sensitivity_weight") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.1   67.0      90.8     90.8
#> 2        1         2    66.3   70.6     286.     286. 
#> 3        1         3    68.2   73.4     257.     257. 

dplyr::tbl(con, "risk_sensitivity_css_params") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    63.6   68.4      120.     120.
#> 2        1         2    63.1   68.0      109.     109.
#> 3        1         3    67.2   72.7      187.     187.

dplyr::tbl(con, "risk_sensitivity_fit_params") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    64.1   67.1      168.     168.
#> 2        1         2    63.0   66.2      160.     160.
#> 3        1         3    63.7   68.0      208.     208.

dplyr::tbl(con, "risk_sensitivity_C_ext") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.7   65.5      199.     199.
#> 2        1         2    62.6   65.3      201.     201.
#> 3        1         3    65.1   68.5      305.     305.

# Compared to baseline risk table
dplyr::tbl(con, "risk") |> dplyr::collect()
#> # A tibble: 3 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    61.9   66.4      115.     115.
#> 2        1         2    64.8   69.4      227.     227.
#> 3        1         3    67.2   73.0      161.     161.

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
