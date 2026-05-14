# Store and retrieve boundary geometries

Boundary information is stored as serialized 'BLOB' objects in the
'boundary' table.

## Usage

``` r
set_boundary(GT, df_list, location = "county", overwrite = FALSE)

get_boundary(GT)
```

## Arguments

- GT:

  GeoTox object.

- df_list:

  Named list of data frames containing boundary geometries as sf
  objects.

- location:

  Name of element in `df_list` that contains location boundary
  information (default "county").

- overwrite:

  Logical indicating whether to overwrite existing 'boundary' table.

## Value

For `set_boundary()`, the same GeoTox object, invisibly. For
`get_boundary()`, a data frame with columns 'id' (boundary name) and
'data' (sf object).

## Details

NOTE: This function requires the `sf` package to be installed.

This function takes a named list of sf objects and stores them in the
database. If a location boundary (default "county") is provided, then
the non-geometry fields will be added to the 'location' table and
replaced with a 'location_id' value so they can be linked to data in the
'sample' table.

## Examples

``` r
# Setup sf objects
county <- sf::st_sf(
  FIPS = c(10000, 20000),
  geometry = sf::st_sfc(sf::st_point(1:2), sf::st_point(3:4))
)
state <- sf::st_sf(
  STATE = "XYZ",
  geometry = sf::st_sfc(sf::st_point(5:6))
)
df_list <- list(county = county, state = state)

# Add boundary to GeoTox database
GT <- GeoTox() |> set_boundary(df_list)

# Open a connection to GeoTox database
con <- get_con(GT)

# Look at created tables

dplyr::tbl(con, "boundary") |> dplyr::collect()
#> # A tibble: 2 × 2
#>   id     data         
#>   <chr>  <list>       
#> 1 county <raw [1,093]>
#> 2 state  <raw [916]>  

dplyr::tbl(con, "location") |> dplyr::collect()
#> # A tibble: 2 × 2
#>      id  FIPS
#>   <int> <dbl>
#> 1     1 10000
#> 2     2 20000

# Retrieve boundary from GeoTox database
boundary <- get_boundary(GT)
boundary
#> # A tibble: 2 × 2
#>   id     data        
#>   <chr>  <list>      
#> 1 county <sf [2 × 2]>
#> 2 state  <sf [1 × 2]>
boundary |> tibble::deframe()
#> $county
#> Simple feature collection with 2 features and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 1 ymin: 2 xmax: 3 ymax: 4
#> CRS:           NA
#>   location_id    geometry
#> 1           1 POINT (1 2)
#> 2           2 POINT (3 4)
#> 
#> $state
#> Simple feature collection with 1 feature and 1 field
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 5 ymin: 6 xmax: 5 ymax: 6
#> CRS:           NA
#>   STATE    geometry
#> 1   XYZ POINT (5 6)
#> 

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
