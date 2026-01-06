# Simulate inhalation rates

Simulate inhalation rates

## Usage

``` r
simulate_inhalation_rate(x, IR_params = NULL)
```

## Arguments

- x:

  numeric vector or list of numeric vectors containing ages.

- IR_params:

  (optional) data frame with columns "age", "mean" and "sd". See details
  for more information.

## Value

List of numeric vectors containing inhalation rates.

## Details

The age column of the optional `IR_params` data frame should be in
ascending order and represent the lower value of age groups for the
corresponding mean and sd values. When not provided, the default values
will come from Table 6.7 of EPA's 2011 Exposure Factors Handbook using
the mean of male and female values.

## Examples

``` r
# Single numeric vector
ages <- sample(1:100, 6, replace = TRUE)
simulate_inhalation_rate(ages)
#> [[1]]
#> [1] 0.1780556 0.3441512 0.3181608 0.2370983 0.2240725 0.2355928
#> 

# List of numeric vectors
ages <- list(
  sample(1:100, 5, replace = TRUE),
  sample(1:100, 3, replace = TRUE)
)
simulate_inhalation_rate(ages)
#> [[1]]
#> [1] 0.2161468 0.2832617 0.2245611 0.1837670 0.4860391
#> 
#> [[2]]
#> [1] 0.1352912 0.3117119 0.2413373
#> 

# Custom IR_params
IR_params <- data.frame("age" = c(0, 20, 50),
                        "mean" = c(0.5, 0.3, 0.2),
                        "sd" = c(0.1, 0.06, 0.03))
simulate_inhalation_rate(c(15, 30, 65), IR_params)
#> [[1]]
#> [1] 0.5673557 0.3498753 0.1712460
#> 
```
