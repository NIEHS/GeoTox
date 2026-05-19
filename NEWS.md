# GeoTox 1.0.0

* Reimplementation where data are now stored in a DuckDB database rather than an
in-memory R object.

* Majority of functions have been redone for the new storage structure.

* Hill curve fits using `fit_hill()` are now bidirectional. Previously only the
positive direction was fit.

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
