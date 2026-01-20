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
#> [1,] 1.0118749 1.825807 3.069007
#> [2,] 0.9985051 1.800722 3.020591
#> [3,] 1.1501370 2.236371 3.359616
#> [4,] 0.9950735 2.208834 3.005477
#> [5,] 0.9447041 2.026918 2.883992
#> 

# List of 2 data frames
y <- data.frame(mean = 4:6, sd = 0.1, casn = letters[1:3])
simulate_exposure(list(loc1 = x, loc2 = y), n = 5)
#> $loc1
#>              a        b        c
#> [1,] 0.9709773 2.107346 2.991774
#> [2,] 0.9372580 2.056798 2.795501
#> [3,] 0.9196467 2.255035 2.817007
#> [4,] 1.0967971 1.869585 3.241197
#> [5,] 1.0738694 1.703378 2.878440
#> 
#> $loc2
#>             a        b        c
#> [1,] 3.917817 4.997201 6.145849
#> [2,] 4.088133 4.923779 6.046758
#> [3,] 3.989283 5.007522 5.985234
#> [4,] 3.925764 5.151380 5.927939
#> [5,] 3.885855 5.076009 6.215276
#> 
# different sample sizes
simulate_exposure(list(loc1 = x, loc2 = y), n = c(5, 3))
#> $loc1
#>              a        b        c
#> [1,] 0.9491920 1.869084 2.921560
#> [2,] 1.0438846 2.086854 2.961969
#> [3,] 1.0773879 2.065955 3.068951
#> [4,] 0.9048719 1.739879 3.014626
#> [5,] 1.0861998 2.375972 3.332553
#> 
#> $loc2
#>             a        b        c
#> [1,] 4.036732 5.013597 5.904727
#> [2,] 3.901093 4.939445 5.916056
#> [3,] 4.282058 4.964946 5.932905
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
#> [1,] 1.0583063 1.780812 3.249216
#> [2,] 0.8311137 2.316820 2.961909
#> [3,] 1.1070061 2.047741 2.777624
#> [4,] 1.2227959 2.081375 2.740206
#> [5,] 1.0441809 2.162632 3.154257
#> 
```
