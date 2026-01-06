# Calculate *in vitro* concentration

Estimate the *in vitro* equivalent plasma concentration given internal
chemical dose and steady-state plasma concentration.

## Usage

``` r
calc_invitro_concentration(D_int, C_ss = NULL)
```

## Arguments

- D_int:

  internal chemical dose in \\\frac{mg}{kg}\\

- C_ss:

  steady-state plasma concentration in \\\frac{\mu M}{mg / kg}\\

## Value

list of matrices containing concentrations in \\\mu M\\

## Details

Input `D_int` must be a matrix or list of matrices. Input `C_ss` must be
a numeric atomic vector or matrix, or a list of those types.

The *in vitro* equivalent plasma concentration is calculated as:
\$\$C\_{plasma} = C\_{ss} \times D\_{int}\$\$

## Examples

``` r
# Single population
D_int <- matrix(1:15, ncol = 3)
C_ss <- 1:5
calc_invitro_concentration(D_int, C_ss)
#> [[1]]
#>      [,1] [,2] [,3]
#> [1,]    1    6   11
#> [2,]    4   14   24
#> [3,]    9   24   39
#> [4,]   16   36   56
#> [5,]   25   50   75
#> 

# Multiple populations
D_int <- list(
  "a" = matrix(1:15 / 10, ncol = 3),
  "b" = matrix(1:8, ncol = 2)
)
C_ss <- list(1:5, 1:4 / 2)
calc_invitro_concentration(D_int, C_ss)
#> $a
#>      [,1] [,2] [,3]
#> [1,]  0.1  0.6  1.1
#> [2,]  0.4  1.4  2.4
#> [3,]  0.9  2.4  3.9
#> [4,]  1.6  3.6  5.6
#> [5,]  2.5  5.0  7.5
#> 
#> $b
#>      [,1] [,2]
#> [1,]  0.5  2.5
#> [2,]  2.0  6.0
#> [3,]  4.5 10.5
#> [4,]  8.0 16.0
#> 
```
