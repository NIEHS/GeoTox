# Changelog

## GeoTox 0.3.0

CRAN release: 2026-01-16

- [`compute_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/compute_sensitivity.md)
  and the wrapper
  [`sensitivity_analysis()`](https://github.com/NIEHS/GeoTox/reference/sensitivity_analysis.md)
  now use fixed Css data with column orders explicitly set to match
  other functions. The column orders are now enforced in the underlying
  functions
  [`get_fixed_age()`](https://github.com/NIEHS/GeoTox/reference/get_fixed_age.md),
  [`get_fixed_obesity()`](https://github.com/NIEHS/GeoTox/reference/get_fixed_obesity.md),
  and
  [`get_fixed_params()`](https://github.com/NIEHS/GeoTox/reference/get_fixed_params.md).

- Added
  [`set_population()`](https://github.com/NIEHS/GeoTox/reference/set_population.md),
  which allows for age and/or obesity status to be set directly and not
  require simulation.

- The simulate\_\* functions can now handle population sizes that vary
  across regions.

## GeoTox 0.2.0

CRAN release: 2024-11-15

- Initial CRAN submission.
