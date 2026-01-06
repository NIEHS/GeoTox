# Set GeoTox boundaries

Set GeoTox boundaries

## Usage

``` r
set_boundaries(x, region = NULL, group = NULL)
```

## Arguments

- x:

  GeoTox object.

- region:

  "sf" data.frame mapping features to a "geometry" column. Used when
  coloring map regions.

- group:

  "sf" data.frame containing a "geometry" column. Used to draw outlines
  around groups of regions.

## Value

same GeoTox object with boundaries set.

## Examples

``` r
geoTox <- GeoTox() |> 
  set_boundaries(region = geo_tox_data$boundaries$county,
                 group  = geo_tox_data$boundaries$state)
```
