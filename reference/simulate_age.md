# Simulate ages

Simulate ages

## Usage

``` r
simulate_age(x, n = 1000)
```

## Arguments

- x:

  data frame or list of data frames containing population data for age
  groups. Each data frame must contain columns "AGEGRP" and "TOT_POP".

- n:

  simulated sample size(s).

## Value

List of arrays containing simulated ages.

## Details

Each data frame must contain 19 rows. The first row represents the total
population of all age groups while the next 18 rows represent age groups
from 0 to 89 in increments of 5 years.

The sample size can be either a single value or a vector the same length
as the number of data frames in x. If a single value is provided, the
same sample size is used for all data frames. If a vector is provided,
each element corresponds to the sample size for each data frame in x.

## Examples

``` r
# Single data frame
x <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
# populate only age range 40-44, set population total of all ages
x$TOT_POP[c(1, 10)] <- 100
simulate_age(x, 5)
#> [[1]]
#> [1] 43 40 40 42 40
#> 

# List of 2 data frames
y <- data.frame(AGEGRP = 0:18, TOT_POP = 0)
# populate age ranges 5-9 and 50-54
y$TOT_POP[c(3, 12)] <- 10
# set population total for all age groups
y$TOT_POP[1] <- sum(y$TOT_POP)
simulate_age(list(x = x, y = y), 15)
#> $x
#>  [1] 44 40 44 40 41 41 42 41 43 40 42 41 42 41 44
#> 
#> $y
#>  [1] 54  5 51  5  6 53 54  9 53  6  8 52  7 52  7
#> 
# different sample sizes
simulate_age(list(x = x, y = y), c(15, 10))
#> $x
#>  [1] 42 43 41 40 41 44 42 40 43 42 44 43 42 40 44
#> 
#> $y
#>  [1] 51  9  9 50  7 54 53  8 54  5
#> 
```
