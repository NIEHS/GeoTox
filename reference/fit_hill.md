# Fit 2- or 3-parameter Hill model

Fit 2- or 3-parameter Hill model

## Usage

``` r
fit_hill(
  x,
  conc = "logc",
  resp = "resp",
  fixed_slope = TRUE,
  chem = NULL,
  assay = NULL
)
```

## Arguments

- x:

  data frame of dose response data.

- conc:

  column name of base-10 log scaled concentration.

- resp:

  column name of response.

- fixed_slope:

  if TRUE, slope is fixed at 1.

- chem:

  (optional) column name of chemical identifiers.

- assay:

  (optional) column name of assay identifiers.

## Value

data frame of fit parameters.

## Details

Optional `chem` and `assay` identifiers can be used to fit multiple
chemicals and/or assays. Returned columns `tp` is the top asymptote and
`logAC50` is the 50% response concentration. If the computation of the
standard deviations of these two parameters fails, then the standard
deviation is set equal to the parameter estimate and is indicated by the
respective imputed flag being TRUE.

## Examples

``` r
# Multiple assays, multiple chemicals
df <- geo_tox_data$dose_response
fit_hill(df, assay = "endp", chem = "casn")
#> # A tibble: 85 × 15
#>    assay   chem     tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min logc_max
#>    <chr>   <chr> <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>    <dbl>
#>  1 APR_He… 510-…  4.44 5.86     2.06      0.448     1        0   -0.398     2.30
#>  2 APR_He… 92-8…  2.14 0.945    2.05      0.271     1        0   -0.398     2.30
#>  3 APR_He… 95-9…  1.46 0.556    1.67      0.246     1        0   -0.699     2   
#>  4 APR_He… 510-…  2.33 0.542    1.79      0.192     1        0   -0.398     2.30
#>  5 APR_He… 72-4…  1.93 1.93     1.95      1.95      1        0   -0.398     2.30
#>  6 APR_He… 92-8…  2.43 1.50     2.22      0.317     1        0   -0.398     2.30
#>  7 ATG_p5… 87-8…  2.15 0.877    1.57      0.321     1        0   -1.30      2   
#>  8 TOX21_… 100-…  0    0        1.45      1.45      1        0   -3         1.95
#>  9 TOX21_… 101-…  0    0        1.45      1.45      1        0   -3         1.95
#> 10 TOX21_… 119-…  0    0        1.10      1.10      1        0   -3         1.95
#> # ℹ 75 more rows
#> # ℹ 5 more variables: resp_min <dbl>, resp_max <dbl>, AIC <dbl>,
#> #   tp.sd.imputed <lgl>, logAC50.sd.imputed <lgl>

# Single assay, multiple chemicals
df <- geo_tox_data$dose_response |>
  dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
fit_hill(df, chem = "casn")
#> # A tibble: 6 × 14
#>   chem     tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min logc_max resp_min
#>   <chr> <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1 119-…  65.6 65.6     2.09     2.09       1        0    -2.70     2.30    -3.01
#> 2 120-… 126.   8.29    1.54     0.0796     1        0    -2.70     2.30    -5.22
#> 3 123-… 186.  17.8     2.21     0.0808     1        0    -2.70     2.30    -3.45
#> 4 510-…  48.5  7.47    2.24     0.123      1        0    -2.70     2.30    -2.38
#> 5 88-0…  58.7 37.0     2.80     0.310      1        0    -2.70     2.30    -4.79
#> 6 92-8…  73.7  7.22    2.06     0.0863     1        0    -2.70     2.30    -4.44
#> # ℹ 4 more variables: resp_max <dbl>, AIC <dbl>, tp.sd.imputed <lgl>,
#> #   logAC50.sd.imputed <lgl>

# Single assay, single chemical
df <- geo_tox_data$dose_response |>
  dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
                casn == "510-15-6")
fit_hill(df)
#> # A tibble: 1 × 13
#>      tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min logc_max resp_min
#>   <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1  48.5  7.47    2.24      0.123     1        0    -2.70     2.30    -2.38
#> # ℹ 4 more variables: resp_max <dbl>, AIC <dbl>, tp.sd.imputed <lgl>,
#> #   logAC50.sd.imputed <lgl>
# 3-parameter Hill model
fit_hill(df, fixed_slope = FALSE)
#> # A tibble: 1 × 13
#>      tp tp.sd logAC50 logAC50.sd slope slope.sd logc_min logc_max resp_min
#>   <dbl> <dbl>   <dbl>      <dbl> <dbl>    <dbl>    <dbl>    <dbl>    <dbl>
#> 1  31.2  1.29    1.83     0.0173  4.02    0.864    -2.70     2.30    -2.38
#> # ℹ 4 more variables: resp_max <dbl>, AIC <dbl>, tp.sd.imputed <lgl>,
#> #   logAC50.sd.imputed <lgl>
```
