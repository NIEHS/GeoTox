# Package index

## GeoTox

Initialize a GeoTox object.

- [`GeoTox()`](https://github.com/NIEHS/GeoTox/reference/GeoTox.md)
  [`get_con()`](https://github.com/NIEHS/GeoTox/reference/GeoTox.md) :
  GeoTox S3 object

## Wrapper functions

Call a single function for each major step of the workflow. Ordered by
workflow sequence.

- [`simulate_population()`](https://github.com/NIEHS/GeoTox/reference/simulate_population.md)
  : Simulate population characteristics and exposures
- [`calc_response()`](https://github.com/NIEHS/GeoTox/reference/calc_response.md)
  : Calculate concentrations and risks
- [`sensitivity_analysis()`](https://github.com/NIEHS/GeoTox/reference/sensitivity_analysis.md)
  : Perform sensitivity analysis

## Simulate population

Generate population characteristics and exposures using Monte Carlo
simulation.

- [`add_age()`](https://github.com/NIEHS/GeoTox/reference/add_age.md) :
  Add age simulation data
- [`add_exposure()`](https://github.com/NIEHS/GeoTox/reference/add_exposure.md)
  : Add exposure simulation data
- [`add_exposure_rate_params()`](https://github.com/NIEHS/GeoTox/reference/add_exposure_rate_params.md)
  : Add exposure rate simulation data
- [`add_obesity()`](https://github.com/NIEHS/GeoTox/reference/add_obesity.md)
  : Add obesity simulation data
- [`simulate_age()`](https://github.com/NIEHS/GeoTox/reference/simulate_age.md)
  : Simulate age values
- [`simulate_exposure()`](https://github.com/NIEHS/GeoTox/reference/simulate_exposure.md)
  : Simulate exposure concentrations
- [`simulate_exposure_rate()`](https://github.com/NIEHS/GeoTox/reference/simulate_exposure_rate.md)
  : Simulate exposure rates
- [`simulate_obesity()`](https://github.com/NIEHS/GeoTox/reference/simulate_obesity.md)
  : Simulate obesity values

### Pre-simulated C_(ss)

Set and sample from pre-simulated steady-state plasma concentrations
C_(ss).

- [`sample_simulated_css()`](https://github.com/NIEHS/GeoTox/reference/sample_simulated_css.md)
  : Sample from pre-simulated steady-state plasma concentrations
- [`set_fixed_css()`](https://github.com/NIEHS/GeoTox/reference/set_fixed_css.md)
  : Prepare steady-state plasma concentrations for sensitivity analysis
- [`set_simulated_css()`](https://github.com/NIEHS/GeoTox/reference/set_simulated_css.md)
  : Set pre-simulated steady-state plasma concentrations

### Set ‘sample’ table

Set population characteristics directly instead of simulating them.

- [`set_sample()`](https://github.com/NIEHS/GeoTox/reference/set_sample.md)
  : Set sample data

## Calculate response

Calculate risk scores using population characteristics, exposures, and
concentration-response data.

- [`calc_internal_dose()`](https://github.com/NIEHS/GeoTox/reference/calc_internal_dose.md)
  : Calculate internal dose
- [`calc_invitro_concentration()`](https://github.com/NIEHS/GeoTox/reference/calc_invitro_concentration.md)
  : Calculate in vitro concentration
- [`calc_risk()`](https://github.com/NIEHS/GeoTox/reference/calc_risk.md)
  : Calculate risk scores

## Sensitivity analysis

- [`calc_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/calc_sensitivity.md)
  : Calculate risk sensitivity to a single variable

## Hill model

- [`add_hill_params()`](https://github.com/NIEHS/GeoTox/reference/add_hill_params.md)
  : Add Hill model parameters
- [`fit_hill()`](https://github.com/NIEHS/GeoTox/reference/fit_hill.md)
  : Fit 2- or 3-parameter Hill model

## Boundary information

- [`set_boundary()`](https://github.com/NIEHS/GeoTox/reference/set_boundary.md)
  [`get_boundary()`](https://github.com/NIEHS/GeoTox/reference/set_boundary.md)
  : Store and retrieve boundary geometries

## Utility

Extract information from a GeoTox object.

- [`get_assay_table()`](https://github.com/NIEHS/GeoTox/reference/get_risk_values.md)
  [`get_risk_quantiles()`](https://github.com/NIEHS/GeoTox/reference/get_risk_values.md)
  [`get_risk_sensitivity()`](https://github.com/NIEHS/GeoTox/reference/get_risk_values.md)
  [`get_risk_values()`](https://github.com/NIEHS/GeoTox/reference/get_risk_values.md)
  : Get risk metric results
- [`get_concentration_mean()`](https://github.com/NIEHS/GeoTox/reference/get_concentration_mean.md)
  : Get concentration mean values

## Datasets

- [`geo_tox_data`](https://github.com/NIEHS/GeoTox/reference/geo_tox_data.md)
  : GeoTox Data
