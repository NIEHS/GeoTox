# Independent Action

Calculate independent action response for a set of chemicals with Hill
concentration-response curves.

## Usage

``` r
calc_independent_action(conc, max, AC50, Emax, n = 1)
```

## Arguments

- conc:

  concentrations in regular space

- max:

  maximal (asymptotic) responses

- AC50:

  concentrations of half-maximal response

- Emax:

  maximum mixture response

- n:

  Hill coefficients (slopes)

## Value

response value

## Details

The concentration is computed as: \$\$ IA = E\_{max} \times \left( 1 -
\prod\limits\_{i} \left(1 - \frac{x_i}{E\_{max}}\right) \right), \$\$
where \\x_i = hill\\val(conc_i, max_i, AC50_i, n_i)\\ is the Hill model
response function for each chemical.

## See also

[`hill_val`](https://github.com/NIEHS/GeoTox/reference/hill_val.md)

## Examples

``` r
n_chem <- 5
conc <- 10^sample(-1:4, n_chem, replace = TRUE)
max <- 80 * runif(n_chem)
AC50 <- 10^(5 * runif(n_chem) - 1)
Emax <- 100

calc_independent_action(conc, max, AC50, Emax)
#> [1] 79.23677
```
