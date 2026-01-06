# Set Hill parameters for a GeoTox object.

Set Hill parameters for a GeoTox object.

## Usage

``` r
set_hill_params(x, hill_params)
```

## Arguments

- x:

  GeoTox object.

- hill_params:

  output of
  [fit_hill](https://github.com/NIEHS/GeoTox/reference/fit_hill.md).

## Value

same GeoTox object with Hill parameters set.

## Examples

``` r
hill_params <- geo_tox_data$dose_response |>
  fit_hill(chem = "casn", assay = "endp") |>
  dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)

geoTox <- GeoTox() |> 
  set_hill_params(hill_params)
```
