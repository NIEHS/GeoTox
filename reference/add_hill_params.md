# Add Hill model parameters

Create or add to the 'hill_params' table in a GeoTox database.

## Usage

``` r
add_hill_params(GT, hill_params)
```

## Arguments

- GT:

  GeoTox object.

- hill_params:

  List output from
  [`fit_hill()`](https://github.com/NIEHS/GeoTox/reference/fit_hill.md).

## Value

The same GeoTox object, invisibly.

## See also

[`fit_hill()`](https://github.com/NIEHS/GeoTox/reference/fit_hill.md)

## Examples

``` r
# Example Hill model data
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
hill_params <- fit_hill(
  hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
)

# Add Hill model parameters to GeoTox database
GT <- GeoTox() |> add_hill_params(hill_params)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "hill_params") |> dplyr::collect()
#> # A tibble: 3 × 16
#>      id assay_id substance_id    tp tp.sd logAC50 logAC50.sd slope slope.sd
#>   <int>    <int>        <int> <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>
#> 1     1        1            1 105.  3.43    1.54      0.0792     1        0
#> 2     2        1            2  62.3 1.96    0.783     0.0643     1        0
#> 3     3        2            1  41.1 0.744   0.539     0.0507     1        0
#> # ℹ 7 more variables: logc_min <dbl>, logc_max <dbl>, resp_min <dbl>,
#> #   resp_max <dbl>, AIC <dbl>, tp.sd.imputed <lgl>, logAC50.sd.imputed <lgl>

dplyr::tbl(con, "assay") |> dplyr::collect()
#> # A tibble: 2 × 3
#>      id name  model
#>   <int> <chr> <chr>
#> 1     1 a1    human
#> 2     2 a2    rat  

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
