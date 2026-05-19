# GeoTox S3 object

Create a GeoTox object and connect to the underlying database.

## Usage

``` r
GeoTox(dbname = tempfile(fileext = ".duckdb"), reset_seed = FALSE, ...)

get_con(GT)
```

## Arguments

- dbname:

  Database file name. Default is a temporary file.

- reset_seed:

  Logical indicating whether to reset the user's global `.Random.seed`
  after certain database operations (default `FALSE`).

- ...:

  Additional arguments passed to
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

- GT:

  GeoTox object.

## Value

For `GeoTox()`, a GeoTox S3 object. For `get_con()`, a database
connection.

## Details

The `dbname` will point to a DuckDB database file. If the file does not
already exist, a new database will be created. Additional arguments
passed via `...` will be forwarded to
[`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html).

The `reset_seed` parameter is necessary for replicating results from the
previous GeoTox implementation. Some database functions create temporary
tables where a random string is appended to the table name. This
advances the `.Random.seed` state and causes functions that use
randomness to produce different results between the previous and current
implementations. Setting `reset_seed = TRUE` will reset the user's
`.Random.seed` to the value it had before certain database operations.

Various parameters will be stored in the 'par' table. These parameters
will also be loaded into the GeoTox object as a list in `GT$par`. The
value for `reset_seed` can only be set on initial GeoTox object creation
and will be loaded from the 'par' table for existing databases.

## Examples

``` r
# Create a GeoTox object
GT <- GeoTox()

# Open a connection to GeoTox database
con <- get_con(GT)

# List database tables
DBI::dbListTables(con)
#> [1] "par"

# Look at the GT parameters
dplyr::tbl(con, "par") |> dplyr::collect()
#> # A tibble: 1 × 3
#>   name       value   idx
#>   <chr>      <chr> <int>
#> 1 reset_seed FALSE     1
str(GT$par)
#> List of 1
#>  $ reset_seed: logi FALSE

# Clean up example
DBI::dbDisconnect(con)
file.remove(GT$db_info$dbdir)
#> [1] TRUE
```
