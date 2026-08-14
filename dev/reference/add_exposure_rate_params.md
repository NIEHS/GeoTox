# Add exposure rate simulation data

Create or add to the 'exposure_rate_params' table in a GeoTox database.

## Usage

``` r
add_exposure_rate_params(
  GT,
  route = "inhalation",
  params = NULL,
  overwrite = FALSE
)
```

## Arguments

- GT:

  GeoTox object.

- route:

  Exposure route (default "inhalation").

- params:

  Data frame with exposure rate parameters. If `NULL`, default
  parameters for the specified `route` will be used (default `NULL`).

- overwrite:

  Logical indicating whether to overwrite existing exposure rate
  parameters for the specified `route` (default `FALSE`).

## Value

The same GeoTox object, invisibly.

## Details

There are two routes with built-in default exposure rate parameters:
"inhalation" and "drinking". If `params` is not provided (`NULL`), the
default parameters for the specified `route` will be used. If `params`
is provided, it must contain columns "age_lb", "age_ub", "mean", and
"sd". The exposure rate parameters are used by
[`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md)
to generate exposure rate samples for each individual in the population.

The build-in parameters come from the following sources:

- Inhalation: [EPA Exposure Factors Handbook (2015), Table
  6.7](https://www.epa.gov/sites/default/files/2015-09/documents/efh-chapter06.pdf)
  in m\\^3\\/kg-day. The "mean" and "sd" values are the average of male
  and female values.

- Drinking: [EPA Exposure Factors Handbook (2019), Table
  3-30](https://www.epa.gov/sites/default/files/2019-02/documents/efh_-_chapter_3_update.pdf)
  in mL/kg-day.

## See also

[`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/dev/reference/simulate_exposure_rate.md)

## Examples

``` r
# Add both default params to GeoTox database
GT <- GeoTox() |>
  add_exposure_rate_params() |>
  add_exposure_rate_params(route = "drinking")

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at relevant tables
params_tbl <- dplyr::tbl(con, "exposure_rate_params") |> dplyr::collect()

params_tbl |> dplyr::filter(route_id == 1)
#> # A tibble: 10 × 6
#>       id route_id age_lb age_ub  mean     sd
#>    <int>    <int>  <dbl>  <dbl> <dbl>  <dbl>
#>  1     1        1      0      1 0.488 0.0775
#>  2     2        1      1      2 0.465 0.07  
#>  3     3        1      2      5 0.44  0.055 
#>  4     4        1      5      7 0.41  0.05  
#>  5     5        1      7     11 0.36  0.06  
#>  6     6        1     11     23 0.285 0.05  
#>  7     7        1     23     30 0.24  0.04  
#>  8     8        1     30     40 0.24  0.035 
#>  9     9        1     40     65 0.22  0.04  
#> 10    10        1     65    150 0.18  0.035 

params_tbl |> dplyr::filter(route_id == 2)
#> # A tibble: 10 × 6
#>       id route_id age_lb age_ub  mean    sd
#>    <int>    <int>  <dbl>  <dbl> <dbl> <dbl>
#>  1    11        2      0      1  43.5  42.5
#>  2    12        2      1      4  46.8  28.1
#>  3    13        2      4      7  37.9  21.8
#>  4    14        2      7     11  26.9  15.3
#>  5    15        2     11     15  20.2  11.6
#>  6    16        2     15     20  16.4   9.6
#>  7    17        2     20     45  18.6  10.7
#>  8    18        2     45     65  22    10.8
#>  9    19        2     65     75  21.9   9.9
#> 10    20        2     75    150  21.6   9.5

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation
#> 2     2 drinking  

# Add custom params with new column (gender), assign to route "custom"
params_df <- tibble::tribble(
  ~age_lb, ~age_ub, ~gender, ~mean, ~sd,
   0, 49, "male",   10, 1,
  50, 99, "male",   20, 1,
   0, 49, "female", 30, 1,
  50, 99, "female", 40, 1
)
GT |> add_exposure_rate_params(params_df, route = "custom")

# Look at updated tables
params_tbl <- dplyr::tbl(con, "exposure_rate_params") |> dplyr::collect()

params_tbl |> dplyr::filter(route_id == 1)
#> # A tibble: 10 × 7
#>       id route_id age_lb age_ub  mean     sd gender
#>    <int>    <int>  <dbl>  <dbl> <dbl>  <dbl> <chr> 
#>  1     1        1      0      1 0.488 0.0775 NA    
#>  2     2        1      1      2 0.465 0.07   NA    
#>  3     3        1      2      5 0.44  0.055  NA    
#>  4     4        1      5      7 0.41  0.05   NA    
#>  5     5        1      7     11 0.36  0.06   NA    
#>  6     6        1     11     23 0.285 0.05   NA    
#>  7     7        1     23     30 0.24  0.04   NA    
#>  8     8        1     30     40 0.24  0.035  NA    
#>  9     9        1     40     65 0.22  0.04   NA    
#> 10    10        1     65    150 0.18  0.035  NA    

params_tbl |> dplyr::filter(route_id == 2)
#> # A tibble: 10 × 7
#>       id route_id age_lb age_ub  mean    sd gender
#>    <int>    <int>  <dbl>  <dbl> <dbl> <dbl> <chr> 
#>  1    11        2      0      1  43.5  42.5 NA    
#>  2    12        2      1      4  46.8  28.1 NA    
#>  3    13        2      4      7  37.9  21.8 NA    
#>  4    14        2      7     11  26.9  15.3 NA    
#>  5    15        2     11     15  20.2  11.6 NA    
#>  6    16        2     15     20  16.4   9.6 NA    
#>  7    17        2     20     45  18.6  10.7 NA    
#>  8    18        2     45     65  22    10.8 NA    
#>  9    19        2     65     75  21.9   9.9 NA    
#> 10    20        2     75    150  21.6   9.5 NA    

params_tbl |> dplyr::filter(route_id == 3)
#> # A tibble: 4 × 7
#>      id route_id age_lb age_ub  mean    sd gender
#>   <int>    <int>  <dbl>  <dbl> <dbl> <dbl> <chr> 
#> 1    21        3      0     49    10     1 male  
#> 2    22        3     50     99    20     1 male  
#> 3    23        3      0     49    30     1 female
#> 4    24        3     50     99    40     1 female

dplyr::tbl(con, "route") |> dplyr::collect()
#> # A tibble: 3 × 2
#>      id route     
#>   <int> <chr>     
#> 1     1 inhalation
#> 2     2 drinking  
#> 3     3 custom    

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
