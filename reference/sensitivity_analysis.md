# Perform sensitivity analysis

Calculate risk sensitivity to all available `vary` parameters in
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md).

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
  [`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md).

## Value

The updated GeoTox object, invisibly.

## Details

Sensitivity is calculated in the order: age, weight, css_params,
fit_params, C_ext. The `max_mult` vector allows specifying different
upper bound multipliers for each parameter.

## See also

[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md)

## Examples

``` r
# Example setup is shown below in \dontrun().
# Pre-generated results will be loaded instead to avoid long example runtime.

if (FALSE) { # \dontrun{
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
  ~assay, ~model, ~casn, ~logc, ~resp,
  "a1", "human", "00-00-1",    0,  10,
  "a1", "human", "00-00-1",    1,  20,
  "a1", "human", "00-00-1",    2,  80,
  "a1", "human", "00-00-1",    3, 100,
  "a1", "human", "00-00-2", -0.5,   5,
  "a1", "human", "00-00-2",  0.5,  20,
  "a1", "human", "00-00-2",  1.5,  55,
  "a1", "human", "00-00-2",  2.5,  60,
  "a2",   "rat", "00-00-1",   -1,   0,
  "a2",   "rat", "00-00-1",    0,  10,
  "a2",   "rat", "00-00-1",    1,  30,
  "a2",   "rat", "00-00-1",    2,  40
)
set.seed(1234)
GT <- GeoTox() |>
  set_sample(sample_df) |>
  set_simulated_css(css_df) |>
  add_exposure_rate_params() |>
  add_hill_params(fit_hill(
    hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
  )) |>
  simulate_population(exposure = exposure_df) |>
  calc_response()

# Perform sensitivity analysis
GT <- GT |> sensitivity_analysis()
} # }

# Load results from pre-generated database for this example
temp_dir <- tempdir()
zip::unzip(
  system.file("extdata", "sensitivity.duckdb.zip", package = "GeoTox"),
  junkpaths = TRUE,
  exdir = temp_dir
)
GT <- GeoTox(paste0(temp_dir, "/sensitivity.duckdb"))

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant table

dplyr::tbl(con, "risk_sensitivity_age") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    64.3   69.0      138.     138.
#> 2        1         2    64.8   69.5      162.     162.
#> 3        1         3    68.8   73.9      315.     315.
#> 4        2         1    39.4   39.4      202.     202.
#> 5        2         2    39.6   39.6      236.     236.
#> 6        2         3    40.5   40.5      633.     633.

dplyr::tbl(con, "risk_sensitivity_weight") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.9   67.7      107.     107.
#> 2        1         2    65.4   69.9      195.     195.
#> 3        1         3    66.7   72.4      154.     154.
#> 4        2         1    38.8   38.8      151.     151.
#> 5        2         2    39.9   39.9      286.     286.
#> 6        2         3    39.9   39.9      304.     304.

dplyr::tbl(con, "risk_sensitivity_css_params") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.2   67.2      91.6     91.6
#> 2        1         2    60.9   65.9      73.3     73.3
#> 3        1         3    66.5   72.1     149.     149. 
#> 4        2         1    38.5   38.5     131.     131. 
#> 5        2         2    37.9   37.9     105.     105. 
#> 6        2         3    39.9   39.9     290.     290. 

dplyr::tbl(con, "risk_sensitivity_fit_params") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    64.5   66.3      205.     205.
#> 2        1         2    61.7   65.4      228.     228.
#> 3        1         3    64.5   68.6      264.     264.
#> 4        2         1    39.8   39.8      122.     122.
#> 5        2         2    39.1   39.1      147.     147.
#> 6        2         3    40.3   40.3      250.     250.

dplyr::tbl(con, "risk_sensitivity_C_ext") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    63.0   66.1      200.     200.
#> 2        1         2    63.2   66.1      226.     226.
#> 3        1         3    65.2   69.0      269.     269.
#> 4        2         1    39.0   39.0      169.     169.
#> 5        2         2    39.2   39.2      183.     183.
#> 6        2         3    40.0   40.0      327.     327.

# Compared to baseline risk table
dplyr::tbl(con, "risk") |> dplyr::collect()
#> # A tibble: 6 × 6
#>   assay_id sample_id GCA.Eff IA.Eff GCA.HQ.10 IA.HQ.10
#>      <int>     <int>   <dbl>  <dbl>     <dbl>    <dbl>
#> 1        1         1    62.0   68.2      59.0     59.0
#> 2        1         2    65.0   69.3     232.     232. 
#> 3        1         3    68.1   73.1     190.     190. 
#> 4        2         1    37.1   37.1      89.1     89.1
#> 5        2         2    38.9   38.9     395.     395. 
#> 6        2         3    38.1   38.1     393.     393. 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
