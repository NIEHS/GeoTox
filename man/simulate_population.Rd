% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_population.R
\name{simulate_population}
\alias{simulate_population}
\title{Simulate population data}
\usage{
simulate_population(
  x,
  age = NULL,
  obesity = NULL,
  exposure = NULL,
  simulated_css = NULL,
  ...
)
}
\arguments{
\item{x}{GeoTox object.}

\item{age}{input \code{x} to function \link{simulate_age}. After simulating ages, the
inhalation rate is subsequently calculated using \link{simulate_inhalation_rate}.}

\item{obesity}{input \code{x} to function \link{simulate_obesity}.}

\item{exposure}{input \code{x} to function \link{simulate_exposure}.}

\item{simulated_css}{input \code{simulated_css} to functions \link{sample_Css} and
\link{get_fixed_css}.}

\item{...}{additional arguments passed to other functions. See details.}
}
\value{
The same object with simulated fields added.
}
\description{
Simulate population data for given input fields
}
\details{
Additional parameters include \code{n} for sample size,
\code{IR_params} for \link{simulate_inhalation_rate},
\code{obes_prev}, \code{obes_sd}, and \code{obes_label} for \link{simulate_obesity},
and \code{expos_mean}, \code{expos_sd}, and \code{expos_label} for \link{simulate_exposure}.
}
\examples{
# For information about geo_tox_data, see:
# vignette("package_data", package = "GeoTox")

geoTox <- GeoTox() |> 
  simulate_population(age = split(geo_tox_data$age, ~FIPS)[1:5],
                      obesity = geo_tox_data$obesity[1:5, ],
                      exposure = split(geo_tox_data$exposure, ~FIPS)[1:5],
                      simulated_css = geo_tox_data$simulated_css,
                      n = 10)
}
