# Hill model concentration

Calculate the concentration in regular space for a given response value.

## Usage

``` r
hill_conc(resp, max, AC50, n)
```

## Arguments

- resp:

  response value

- max:

  maximal (asymptotic) response

- AC50:

  concentration of half-maximal response

- n:

  Hill coefficient (slope)

## Value

concentration in regular space

## Details

This is a regular space version of
[tcpl::tcplHillConc()](https://cran.r-project.org/package=tcpl).

The concentration is computed as: \$\$conc = AC50 \* (\frac{max}{resp} -
1)^{-1 / n}\$\$

## See also

[`hill_val`](https://github.com/NIEHS/GeoTox/reference/hill_val.md)

## Examples

``` r
hill_conc(c(0.2, 0.5, 0.75), 1, 0.01, 1)
#> [1] 0.0025 0.0100 0.0300
hill_conc(c(0.2, 0.5, 0.9), 1, c(0.1, 0.01, 0.001), 2)
#> [1] 0.050 0.010 0.003
```
