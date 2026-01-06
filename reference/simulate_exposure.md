# Simulate external exposure

Simulate external exposure

## Usage

``` r
simulate_exposure(
  x,
  expos_mean = "mean",
  expos_sd = "sd",
  expos_label = "casn",
  n = 1000
)
```

## Arguments

- x:

  data frame or list of data frames containing exposure data.

- expos_mean:

  column name of mean values.

- expos_sd:

  column name of standard deviations.

- expos_label:

  column name of labeling term, required if `x` has more than one row.

- n:

  simulated sample size(s).

## Value

list of matrices containing inhalation rates. Matrix columns are named
using the values in the `expos_label` column for more than one data
frame row. Columns are sorted to have consistent order across functions.

## Details

The sample size can be either a single value or a vector the same length
as the number of data frames in x. If a single value is provided, the
same sample size is used for all data frames. If a vector is provided,
each element corresponds to the sample size for each data frame in x.

## Examples

``` r
# Single data frame
x <- data.frame(mean = 1:3, sd = (1:3) / 10, casn = letters[1:3])
simulate_exposure(x, n = 5)
#> [[1]]
#>              a        b        c
#> [1,] 0.9002923 1.622919 2.543950
#> [2,] 1.1156145 2.199032 2.897253
#> [3,] 1.1046938 1.949350 3.341872
#> [4,] 1.0485002 1.901233 2.789170
#> [5,] 1.0724220 1.701724 2.883550
#> 

# List of 2 data frames
y <- data.frame(mean = 4:6, sd = 0.1, casn = letters[1:3])
simulate_exposure(list(loc1 = x, loc2 = y), n = 5)
#> $loc1
#>              a        b        c
#> [1,] 0.9188285 2.318611 3.369753
#> [2,] 1.0237200 2.073822 2.870037
#> [3,] 0.7906658 2.119244 2.772665
#> [4,] 1.0660787 1.921109 3.379063
#> [5,] 1.0042271 1.843546 2.828725
#> 
#> $loc2
#>             a        b        c
#> [1,] 4.180368 4.905022 5.900303
#> [2,] 4.148468 5.025342 6.075056
#> [3,] 3.970195 4.885813 5.959528
#> [4,] 3.864046 4.932410 6.013644
#> [5,] 4.132981 4.907200 5.907231
#> 
# different sample sizes
simulate_exposure(list(loc1 = x, loc2 = y), n = c(5, 3))
#> $loc1
#>              a        b        c
#> [1,] 0.9613434 1.873443 3.323238
#> [2,] 1.0378938 1.780600 3.350027
#> [3,] 1.0444706 2.175394 3.118777
#> [4,] 1.0040433 2.229786 2.881321
#> [5,] 0.9602095 1.898961 2.546025
#> 
#> $loc2
#>             a        b        c
#> [1,] 3.975872 4.876136 6.081688
#> [2,] 4.058237 4.966861 5.958555
#> [3,] 3.975649 5.207861 6.109339
#> 

# Input has custom column names
z <- data.frame(ave = 1:3, stdev = (1:3) / 10, chnm = letters[1:3])
simulate_exposure(z,
                  expos_mean = "ave",
                  expos_sd = "stdev",
                  expos_label = "chnm",
                  n = 5)
#> [[1]]
#>              a        b        c
#> [1,] 1.0208125 2.096904 2.956041
#> [2,] 0.9829094 1.868546 2.720801
#> [3,] 0.9492028 1.782699 2.793559
#> [4,] 1.0100442 2.011380 3.205342
#> [5,] 0.8644066 1.782694 2.792641
#> 
```
