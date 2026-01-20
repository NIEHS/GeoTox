# Simulate obesity status

Simulate obesity status

## Usage

``` r
simulate_obesity(
  x,
  obes_prev = "OBESITY_CrudePrev",
  obes_sd = "OBESITY_SD",
  obes_label = "FIPS",
  n = 1000
)
```

## Arguments

- x:

  data frame containing obesity data as a percentage from 0 to 100.

- obes_prev:

  column name of prevalence.

- obes_sd:

  column name of standard deviation.

- obes_label:

  column name of labeling term, required if `x` has more than one row.

- n:

  simulated sample size(s).

## Value

List of arrays containing simulated obesity status.

## Details

The sample size can be either a single value or a vector the same length
as the number of rows in x. If a single value is provided, the same
sample size is used for all data frames. If a vector is provided, each
element corresponds to the sample size for each row in x.

## Examples

``` r
# Input has default column names
df <- data.frame(OBESITY_CrudePrev = c(20, 50, 80),
                 OBESITY_SD = c(5, 5, 5),
                 FIPS = letters[1:3])
simulate_obesity(df, n = 5)
#> $a
#> [1] "Normal" "Obese"  "Normal" "Normal" "Normal"
#> 
#> $b
#> [1] "Obese"  "Obese"  "Obese"  "Obese"  "Normal"
#> 
#> $c
#> [1] "Obese"  "Obese"  "Normal" "Obese"  "Obese" 
#> 
# different sample sizes
simulate_obesity(df, n = 5:3)
#> $a
#> [1] "Normal" "Normal" "Normal" "Normal" "Obese" 
#> 
#> $b
#> [1] "Normal" "Obese"  "Obese"  "Obese" 
#> 
#> $c
#> [1] "Obese" "Obese" "Obese"
#> 

# Input has custom column names
df <- data.frame(prev = c(20, 50, 80),
                 sd = c(5, 5, 5),
                 label = letters[1:3])
simulate_obesity(df,
                 obes_prev = "prev",
                 obes_sd = "sd",
                 obes_label = "label",
                 n = 5)
#> $a
#> [1] "Normal" "Normal" "Normal" "Normal" "Normal"
#> 
#> $b
#> [1] "Obese"  "Normal" "Normal" "Obese"  "Obese" 
#> 
#> $c
#> [1] "Obese" "Obese" "Obese" "Obese" "Obese"
#> 
```
