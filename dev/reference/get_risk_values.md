# Get risk metric results

Several functions used to fetch risk metric results from risk tables in
a GeoTox database. The outputs of these functions are useful for
plotting or further analysis.

## Usage

``` r
get_assay_table(GT)

get_risk_quantiles(
  GT,
  metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
  quantiles = c(0.1, 0.25, 0.5, 0.75, 0.9),
  table_name = "risk"
)

get_risk_sensitivity(
  GT,
  metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
  assay = NULL
)

get_risk_values(
  GT,
  metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
  assay = NULL,
  table_name = "risk"
)
```

## Arguments

- GT:

  GeoTox object.

- metric:

  Risk metric to retrieve. One of "GCA.Eff", "IA.Eff", "GCA.HQ.10", or
  "IA.HQ.10".

- quantiles:

  Numeric vector of quantiles to calculate.

- table_name:

  Name of the risk table to query (default "risk").

- assay:

  Named vector specifying an assay filter (default NULL).

## Value

A data frame or vector.

## Details

Normally an 'assay' table is created when adding the Hill model
concentration-response fit parameters with
[`add_hill_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_hill_params.md).
If no `assay` input is specified in
[`fit_hill()`](https://github.com/NIEHS/GeoTox/dev/reference/fit_hill.md),
then an 'assay' table will not be created. For `get_risk_values()`, the
`assay` parameter can be used to filter results based on assay details
stored in the 'assay' table. This is useful when multiple assays are
available in the database. The `assay` input should be a named vector
specifying the column name and value to filter by, e.g.
`assay = c(endp = "mortality")`. For `get_risk_quantiles()`, if there is
no assay data in the GeoTox database, then the output "assay_id" column
will be filled with NA values. If there is assay data, use
`get_assay_table()` to retrieve assay information and link the
"assay_id" to assay details.

Use the `table_name` parameter to specify which risk table to query.
There can be several risk tables in a GeoTox database. The default table
is named "risk" and is created by either
[`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md)
or the wrapper function
[`calc_response()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_response.md).
Sensitivity analysis results created by either
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md)
or the wrapper function
[`sensitivity_analysis()`](https://github.com/NIEHS/GeoTox/dev/reference/sensitivity_analysis.md)
are stored in other tables with names like "risk_sensitivity_age", etc.
Refer to
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md)
to see which tables may be available.

`get_risk_sensitivity()` is a wrapper function for `get_risk_values()`
around all risk tables created by the original risk computation and
subsequent sensitivity analysis. The column names are "baseline" for the
original risk table, while the other column names correspond to the
parameter varied in the sensitivity analysis.

## See also

[`calc_risk()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_risk.md),
[`calc_response()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_response.md),
[`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/dev/reference/calc_sensitivity.md),
[`sensitivity_analysis()`](https://github.com/NIEHS/GeoTox/dev/reference/sensitivity_analysis.md),
[`fit_hill()`](https://github.com/NIEHS/GeoTox/dev/reference/fit_hill.md),
[`add_hill_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_hill_params.md)

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
  calc_response() |>
  sensitivity_analysis()
} # }

# Load results from pre-generated database for this example
temp_dir <- tempdir()
zip::unzip(
  system.file("extdata", "sensitivity.duckdb.zip", package = "GeoTox"),
  junkpaths = TRUE,
  exdir = temp_dir
)
GT <- GeoTox(paste0(temp_dir, "/sensitivity.duckdb"))

# Look at 'assay' table contents
get_assay_table(GT)
#> # A tibble: 2 × 3
#>      id name  model
#>   <int> <chr> <chr>
#> 1     1 a1    human
#> 2     2 a2    rat  

# Get "GCA.HQ.10" values from 'risk' table for the "a1" assay
get_risk_values(GT, metric = "GCA.HQ.10", assay = c(name = "a1"))
#> [1]  59.02276 232.12433 190.28473

# Get "IA.Eff" values from all risk tables for the "a2" assay
get_risk_sensitivity(GT, metric = "IA.Eff", assay = c(name = "a2"))
#> # A tibble: 3 × 6
#>   C_ext css_params weight   age fit_params baseline
#>   <dbl>      <dbl>  <dbl> <dbl>      <dbl>    <dbl>
#> 1  39.0       38.5   38.8  39.4       39.8     37.1
#> 2  39.2       37.9   39.9  39.6       39.1     38.9
#> 3  40.0       39.9   39.9  40.5       40.3     38.1

# Get "GCA.Eff" quantiles from 'risk' table
get_risk_quantiles(GT, metric = "GCA.Eff", quantiles = c(0.25, 0.5, 0.75))
#> # A tibble: 12 × 4
#>    assay_id location_id quantile value
#>       <int>       <int>    <dbl> <dbl>
#>  1        1           1     0.25  62.8
#>  2        1           1     0.5   63.5
#>  3        1           1     0.75  64.3
#>  4        1           2     0.25  68.1
#>  5        1           2     0.5   68.1
#>  6        1           2     0.75  68.1
#>  7        2           1     0.25  37.6
#>  8        2           1     0.5   38.0
#>  9        2           1     0.75  38.4
#> 10        2           2     0.25  38.1
#> 11        2           2     0.5   38.1
#> 12        2           2     0.75  38.1

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at the 'risk' table contents
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
