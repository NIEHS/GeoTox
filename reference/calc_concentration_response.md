# Calculate the mixture response from one of three different approaches: IA, GCA, or Hazard Quotient

Calculate the combined response of multiple chemicals. It calculates the
generalized concentration addition response, the independent action
response, and a hazard quotient

## Usage

``` r
calc_concentration_response(
  C_invitro,
  hill_params,
  max_mult = 1.5,
  fixed = FALSE
)
```

## Arguments

- C_invitro:

  in vitro concentrations

- hill_params:

  output from
  [`fit_hill()`](https://github.com/NIEHS/GeoTox/reference/fit_hill.md)

- max_mult:

  upper bound multiplier for max response

- fixed:

  if TRUE, sd = 0

## Value

list of data frames

## Examples

``` r
C_invitro <- list(
  matrix(1:8 / 1e3, ncol = 2, dimnames = list(NULL, c("c1", "c2"))),
  matrix(9:16 / 1e3, ncol = 2, dimnames = list(NULL, c("c1", "c2")))
)
hill_params <- fit_hill(
  data.frame(chem = rep(c("c1", "c2"), each = 3),
             logc = c(-1, 0, 1, 0, 1, 2),
             resp = c(10, 5, 0, 4, 2, 0) / 10),
  chem = "chem"
)

calc_concentration_response(C_invitro, hill_params)
#> [[1]]
#>   sample     GCA.Eff      IA.Eff   GCA.HQ.10    IA.HQ.10
#> 1      1 0.183849941 0.183893327  5.28553221  5.28563746
#> 2      2 0.484278834 0.489458143 13.26584330 13.26572506
#> 3      3 0.023988052 0.024449693  1.16115966  1.16117227
#> 4      4 0.001461523 0.001462722  0.04062655  0.04062632
#> 
#> [[2]]
#>   sample     GCA.Eff      IA.Eff  GCA.HQ.10    IA.HQ.10
#> 1      1 0.001218893 0.001220453  0.0475080  0.04750807
#> 2      2 0.021832508 0.021930591  0.2312863  0.23128746
#> 3      3 0.110684704 0.115328461 11.0633429 11.06323273
#> 4      4 0.045493261 0.046659088  2.1877855  2.18778258
#> 
calc_concentration_response(C_invitro, hill_params, fixed = TRUE)
#> [[1]]
#>   sample   GCA.Eff    IA.Eff GCA.HQ.10 IA.HQ.10
#> 1      1 0.2393398 0.2742427  13.49985     13.5
#> 2      2 0.3102525 0.3493325  23.39997     23.4
#> 3      3 0.3479706 0.3866123  33.29939     33.3
#> 4      4 0.3713827 0.4087487  43.19904     43.2
#> 
#> [[2]]
#>   sample   GCA.Eff    IA.Eff GCA.HQ.10 IA.HQ.10
#> 1      1 0.4200784 0.4517192  92.70122     92.7
#> 2      2 0.4246337 0.4554551 102.60133    102.6
#> 3      3 0.4284513 0.4585306 112.50143    112.5
#> 4      4 0.4316926 0.4611019 122.40154    122.4
#> 
```
