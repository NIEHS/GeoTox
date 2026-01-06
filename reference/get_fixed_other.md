# Get median `C_ss` Values

Get median `C_ss` Values

## Usage

``` r
get_fixed_other(C_ss)
```

## Arguments

- C_ss:

  list of matrices containing `C_ss` data

## Value

list of atomic vectors containing median `C_ss` values.

## Examples

``` r
# Generate input C_ss data
age <- list(c(25, 35, 55),
            c(15, 60))
obesity <- list(c("Obese", "Normal", "Obese"),
                c("Normal", "Normal"))
C_ss <- sample_Css(simulated_css = geo_tox_data$simulated_css,
                   age = age,
                   obesity = obesity)

# Get median C_ss values
get_fixed_other(C_ss)
#> [[1]]
#> [1] 2.308 2.308 2.308
#> 
#> [[2]]
#> [1] 1.584 1.584
#> 
```
