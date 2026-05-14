# Fit 2- or 3-parameter Hill model

Fit a 2-parameter (fixed slope) or 3-parameter (variable slope) Hill
model to concentration-response data.

## Usage

``` r
fit_hill(
  x,
  conc = "logc",
  resp = "resp",
  fixed_slope = TRUE,
  assay = NULL,
  substance = NULL
)
```

## Arguments

- x:

  Data frame of dose response data.

- conc:

  Column name of base-10 log scaled concentration (default "logc").

- resp:

  Column name of response (default "resp").

- fixed_slope:

  Logical indicating whether to fit a 2-parameter (TRUE) or 3-parameter
  (FALSE) Hill model (default TRUE).

- assay:

  Column name of assay identifier(s) (optional, default NULL).

- substance:

  Column name of substance identifier(s) (optional, default NULL).

## Value

A list with elements 'fit', 'assay', 'substance'. The 'fit' element is a
data frame of fit parameters, while the 'assay' and 'substance' elements
indicate the column names used for assay and substance identifiers,
respectively.

## Details

The input `x` data frame must contain columns specified by `conc` and
`resp` arguments representing the log10-transformed concentration and
response, respectively.

Optional `assay` and `substance` identifiers can be named vectors that
are used to fit multiple substances and/or assays. For example,
`assay = c(name = "assay", "model")` would indicate that `x` contains
both and assay name and model. The `name = "assay"` part would rename
the "assay" column in `x` to "name" in the 'assay' table when aded with
[`add_hill_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_hill_params.md).

Returned column 'tp' is the top asymptote and 'logAC50' is the 50%
response concentration. If the computation of the standard deviations of
these two parameters fails, then the standard deviation is set equal to
the parameter estimate and is indicated by the respective imputed flag
being TRUE.

## See also

[`add_hill_params()`](https://github.com/NIEHS/GeoTox/dev/reference/add_hill_params.md)

## Examples

``` r
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

# Fit 2-parameter Hill model
fit_hill(
  hill_df, assay = c(name = "assay", model = "model"), substance = "casn"
)
#> $fit
#> # A tibble: 3 × 16
#>   assay model casn       tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min
#>   <chr> <chr> <chr>   <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>
#> 1 a1    human 00-00-1 105.  3.43    1.54      0.0792     1        0      0  
#> 2 a1    human 00-00-2  62.3 1.96    0.783     0.0643     1        0     -0.5
#> 3 a2    rat   00-00-1  41.1 0.744   0.539     0.0507     1        0     -1  
#> # ℹ 6 more variables: logc_max <dbl>, resp_min <dbl>, resp_max <dbl>,
#> #   AIC <dbl>, tp.sd.imputed <lgl>, logAC50.sd.imputed <lgl>
#> 
#> $assay
#>    name   model 
#> "assay" "model" 
#> 
#> $substance
#> [1] "casn"
#> 

# Fit 3-parameter Hill model
fit_hill(hill_df, assay = "assay", substance = "casn", fixed_slope = FALSE)
#> $fit
#> # A tibble: 3 × 15
#>   assay casn       tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min logc_max
#>   <chr> <chr>   <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#> 1 a1    00-00-1 102.  2.39    1.52      0.0410 1.16    0.0900      0        3  
#> 2 a1    00-00-2  60.5 1.05    0.736     0.0309 1.27    0.125      -0.5      2.5
#> 3 a2    00-00-1  41.8 0.622   0.557     0.0254 0.919   0.0421     -1        2  
#> # ℹ 5 more variables: resp_min <dbl>, resp_max <dbl>, AIC <dbl>,
#> #   tp.sd.imputed <lgl>, logAC50.sd.imputed <lgl>
#> 
#> $assay
#> [1] "assay"
#> 
#> $substance
#> [1] "casn"
#> 
```
