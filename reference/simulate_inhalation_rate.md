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
#> [1] 0.2932500 0.2185389 0.2775303 0.1959811 0.2619654 0.2890411
#> 

# List of numeric vectors
ages <- list(
  sample(1:100, 5, replace = TRUE),
  sample(1:100, 3, replace = TRUE)
)
simulate_inhalation_rate(ages)
#> [[1]]
#> [1] 0.1771644 0.3518773 0.1941855 0.2418979 0.1494693
#> 
#> [[2]]
#> [1] 0.2621927 0.1900576 0.1580984
#> 

# Custom IR_params
IR_params <- data.frame("age" = c(0, 20, 50),
                        "mean" = c(0.5, 0.3, 0.2),
                        "sd" = c(0.1, 0.06, 0.03))
simulate_inhalation_rate(c(15, 30, 65), IR_params)
#> [[1]]
#> [1] 0.4052714 0.2191278 0.2305595
#> 
```
