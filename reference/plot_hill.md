# Plot Hill equation fits.

Plot Hill equation fits.

## Usage

``` r
plot_hill(hill_params, xlim = c(-1, 4))
```

## Arguments

- hill_params:

  output from
  [`fit_hill`](https://github.com/NIEHS/GeoTox/reference/fit_hill.md).

- xlim:

  log-10 scaled concentration limits.

## Value

ggplot2 object.

## Examples

``` r
# Multiple assays, multiple chemicals
df <- geo_tox_data$dose_response
plot_hill(fit_hill(df, assay = "endp", chem = "casn"))


# Single assay, multiple chemicals
df <- geo_tox_data$dose_response |>
  dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio")
fig <- plot_hill(fit_hill(df, chem = "casn"))
fig

# Modify plot
fig + ggplot2::guides(color = ggplot2::guide_legend(title = "Chemical\nCASN"))


# Single assay, single chemical
df <- geo_tox_data$dose_response |>
  dplyr::filter(endp == "TOX21_H2AX_HTRF_CHO_Agonist_ratio",
                casn == "510-15-6")
plot_hill(fit_hill(df))

# 3-parameter Hill model
plot_hill(fit_hill(df, fixed_slope = FALSE))
```
