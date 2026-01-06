# Calculate internal chemical dose

Estimate the internal dose from inhalation of a chemical given
inhalation rate, time, and body weight

## Usage

``` r
calc_internal_dose(C_ext, IR, time = 1, BW = 1, scaling = 1)
```

## Arguments

- C_ext:

  ambient chemical concentration in \\\frac{mg}{m^3}\\

- IR:

  inhalation rate in \\\frac{m^3}{day}\\

- time:

  total time in \\days\\

- BW:

  body weight in \\kg\\

- scaling:

  scaling factor encompassing any required unit adjustments

## Value

list of matrices containing internal chemical doses in \\\frac{mg}{kg}\\

## Details

Input `C_ext` must be a matrix or list of matrices. Input `IR` must be
an atomic vector or list of atomic vectors. The `time`, `BW` and
`scaling` arguments are scalars.

The internal dose is calculated as: \$\$D\_{int} = \frac{C\_{ext} \times
IR \times time}{BW} \times scaling\$\$

## Examples

``` r
# Single population
C_ext <- matrix(1:15, ncol = 3)
IR <- 1:5
calc_internal_dose(C_ext, IR)
#> [[1]]
#>      [,1] [,2] [,3]
#> [1,]    1    6   11
#> [2,]    4   14   24
#> [3,]    9   24   39
#> [4,]   16   36   56
#> [5,]   25   50   75
#> 

# Multiple populations
C_ext <- list(
  "a" = matrix(1:15 / 10, ncol = 3),
  "b" = matrix(1:8, ncol = 2)
)
IR <- list(1:5, 1:4 / 2)
calc_internal_dose(C_ext, IR)
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
