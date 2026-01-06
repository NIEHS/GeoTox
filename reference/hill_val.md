# Hill model response

Calculate the response for a given concentration in regular space.

## Usage

``` r
hill_val(conc, max, AC50, n)
```

## Arguments

- conc:

  concentration in regular space

- max:

  maximal (asymptotic) response

- AC50:

  concentration of half-maximal response

- n:

  Hill coefficient (slope)

## Value

response value

## Details

This is a regular space version of
[tcpl::tcplHillVal()](https://cran.r-project.org/package=tcpl).

The Hill model is defined as: \$\$resp = \frac{max}{1 +
(\frac{AC50}{conc})^{n}}\$\$

## See also

[`hill_conc`](https://github.com/NIEHS/GeoTox/reference/hill_conc.md)

## Examples

``` r
hill_val(c(0.0025, 0.01, 0.03), 1, 0.01, 1)
#> [1] 0.20 0.50 0.75
hill_val(c(0.05, 0.01, 0.003), 1, c(0.1, 0.01, 0.001), 2)
#> [1] 0.2 0.5 0.9
```
