# Get response quantiles

Get response quantiles

## Usage

``` r
resp_quantiles(
  resp,
  metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
  assays = NULL,
  assay_summary = FALSE,
  assay_quantiles = c(Median = 0.5),
  summary_quantiles = c(`10th percentile` = 0.1)
)
```

## Arguments

- resp:

  calculated mixture response output from
  [calc_concentration_response](https://github.com/NIEHS/GeoTox/reference/calc_concentration_response.md).

- metric:

  response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10" or
  "IA.HQ.10".

- assays:

  assays to summarize. If NULL and multiple assays exist, then the first
  assay is used.

- assay_summary:

  boolean indicating whether to summarize across assays.

- assay_quantiles:

  numeric vector of assay quantiles.

- summary_quantiles:

  numeric vector of quantiles to compute across all assay quantiles.

## Value

data frame with computed response quantiles.

## Details

The columns of the returned data frame will vary based on the inputs. If
assays is specified and assay_summary is FALSE, then the resulting data
frame will have an assay column. If assay_summary is TRUE, then the data
frame will have an summary_quantile column.

## Examples

``` r
# Dummy response data
resp <- list(
  "r1" = data.frame(assay = c("a1", "a1", "a2", "a2"),
                    sample = c(1, 2, 1, 2),
                    GCA.Eff = c(1, 2, 3, 4),
                    IA.Eff = c(5, 6, 7, 8),
                    "GCA.HQ.10" = c(9, 10, 11, 12),
                    "IA.HQ.10" = c(13, 14, 15, 16)))

# Summarize single assay
resp_quantiles(resp)
#> Warning: Multiple assays found, using first assay 'a1'
#> # A tibble: 1 × 5
#>   id    assay metric  assay_quantile value
#>   <chr> <chr> <chr>            <dbl> <dbl>
#> 1 r1    a1    GCA.Eff            0.5   1.5
# Specify assay
resp_quantiles(resp, assays = "a1")
#> # A tibble: 1 × 5
#>   id    assay metric  assay_quantile value
#>   <chr> <chr> <chr>            <dbl> <dbl>
#> 1 r1    a1    GCA.Eff            0.5   1.5
# Specify quantiles
resp_quantiles(resp, assays = "a1", assay_quantiles = c(0.25, 0.75))
#> # A tibble: 2 × 5
#>   id    assay metric  assay_quantile value
#>   <chr> <chr> <chr>            <dbl> <dbl>
#> 1 r1    a1    GCA.Eff           0.25  1.25
#> 2 r1    a1    GCA.Eff           0.75  1.75
# Specify metric
resp_quantiles(resp, assays = "a1", metric = "IA.HQ.10")
#> # A tibble: 1 × 5
#>   id    assay metric   assay_quantile value
#>   <chr> <chr> <chr>             <dbl> <dbl>
#> 1 r1    a1    IA.HQ.10            0.5  13.5

# Summarize across assays
resp_quantiles(resp, assay_summary = TRUE)
#> Warning: There are only 2 assays.
#> Consider using a larger number of assays for a more robust analysis.
#> # A tibble: 1 × 5
#>   id    assay_quantile metric  summary_quantile value
#>   <chr>          <dbl> <chr>              <dbl> <dbl>
#> 1 r1               0.5 GCA.Eff              0.1   1.7
# Specify quantiles
suppressWarnings(
  resp_quantiles(resp,
                 assay_summary = TRUE,
                 assay_quantiles = c(0.25, 0.75),
                 summary_quantiles = c(0.1, 0.9))
)
#> # A tibble: 4 × 5
#>   id    assay_quantile metric  summary_quantile value
#>   <chr>          <dbl> <chr>              <dbl> <dbl>
#> 1 r1              0.25 GCA.Eff              0.1  1.45
#> 2 r1              0.25 GCA.Eff              0.9  3.05
#> 3 r1              0.75 GCA.Eff              0.1  1.95
#> 4 r1              0.75 GCA.Eff              0.9  3.55
```
