# GeoTox (development version)

# GeoTox 0.3.0

* `compute_sensitivity()` and the wrapper `sensitivity_analysis()` now use fixed
Css data with column orders explicitly set to match other functions. The column
orders are now enforced in the underlying functions `get_fixed_age()`,
`get_fixed_obesity()`, and `get_fixed_params()`.

* Added `set_population()`, which allows for age and/or obesity status to be set
directly and not require simulation.

* The simulate\_\* functions can now handle population sizes that vary
across regions.

# GeoTox 0.2.0

* Initial CRAN submission.
