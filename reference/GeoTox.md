# GeoTox S3 object

An S3 object that can be used to help organize the data and results of a
GeoTox analysis.

## Usage

``` r
GeoTox()

# S3 method for class 'GeoTox'
plot(x, type = c("resp", "hill", "exposure", "sensitivity"), ...)
```

## Arguments

- x:

  GeoTox object.

- type:

  type of plot.

- ...:

  arguments passed to subsequent methods.

## Value

a GeoTox S3 object

## See also

[plot_resp](https://github.com/NIEHS/GeoTox/reference/plot_resp.md),
[plot_hill](https://github.com/NIEHS/GeoTox/reference/plot_hill.md),
[plot_exposure](https://github.com/NIEHS/GeoTox/reference/plot_exposure.md),
[plot_sensitivity](https://github.com/NIEHS/GeoTox/reference/plot_sensitivity.md)

## Examples

``` r
# Use a subset of the package data for demonstration purposes
set.seed(2357)
n <- 10 # Population size
m <- 5 # Number of regions
idx <- if (m < 100) sample(1:100, m) else 1:100

geoTox <- GeoTox() |> 
  # Set region and group boundaries (for plotting)
  set_boundaries(region = geo_tox_data$boundaries$county,
                 group  = geo_tox_data$boundaries$state) |> 
  # Simulate populations for each region
  simulate_population(age           = split(geo_tox_data$age, ~FIPS)[idx],
                      obesity       = geo_tox_data$obesity[idx, ],
                      exposure      = split(geo_tox_data$exposure, ~FIPS)[idx],
                      simulated_css = geo_tox_data$simulated_css,
                      n             = n) |> 
  # Estimated Hill parameters
  set_hill_params(geo_tox_data$dose_response |>
                    fit_hill(assay = "endp", chem = "casn") |> 
                    dplyr::filter(!tp.sd.imputed, !logAC50.sd.imputed)) |>
  # Calculate response
  calculate_response() |>
  # Perform sensitivity analysis
  sensitivity_analysis()

# Print GeoTox object
geoTox
#> GeoTox object
#> Assays: 13
#> Chemicals: 20
#> Regions: m = 5
#> Population: n = 10
#> Data Fields:
#>      Name          Size
#>       age       m * (n)
#>        IR       m * (n)
#>   obesity       m * (n)
#>     C_ext  m * (n x 21)
#>      C_ss  m * (n x 21)
#> Computed Fields:
#>          Name                    Size
#>         D_int            m * (n x 21)
#>     C_invitro            m * (n x 21)
#>          resp        m * (13 * n x 6)
#>   sensitivity  5 * (m * (13 * n x 6))
#> Other Fields: par, boundaries, exposure, css_sensitivity, hill_params

# Plot hill fits
plot(geoTox, type = "hill")

# Plot exposure data
plot(geoTox, type = "exposure", ncol = 5)

# Plot response data
plot(geoTox, assays = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")

# Plot sensitivity data
plot(geoTox,
     type = "sensitivity",
     assay = "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
#> Picking joint bandwidth of 0.533
```
