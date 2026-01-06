# Set population data

Set population data

## Usage

``` r
set_population(x, age = NULL, obesity = NULL)
```

## Arguments

- x:

  GeoTox object.

- age:

  numeric vector or list of numeric vectors of age values.

- obesity:

  character vector or list of character vectors of obesity status.

## Value

The same object with simulated fields added.

## Examples

``` r
# Single region
age <- round(runif(10, 1, 100))
obesity <- sample(c("Normal", "Obese"), 10, replace = TRUE)
geoTox <- set_population(GeoTox(), age = age, obesity = obesity)

# Multiple regions
age <- list("37001" = round(runif(10, 1, 100)),
            "37007" = round(runif(8, 1, 100)))
obesity <- list("37001" = sample(c("Normal", "Obese"), 10, replace = TRUE),
                "37007" = sample(c("Normal", "Obese"), 8, replace = TRUE))
geoTox <- set_population(GeoTox(), age = age, obesity = obesity)
```
