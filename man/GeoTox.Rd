% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoTox.R
\name{GeoTox}
\alias{GeoTox}
\alias{plot.GeoTox}
\title{GeoTox S3 object}
\usage{
GeoTox()

\method{plot}{GeoTox}(x, type = c("resp", "hill", "exposure", "sensitivity"), ...)
}
\arguments{
\item{x}{GeoTox object}

\item{type}{type of plot}

\item{...}{additional arguments passed to plotting functions}
}
\description{
An S3 object that can be used to help organize the data and results of a
GeoTox analysis.
}
\seealso{
\link{plot_resp}, \link{plot_hill}, \link{plot_exposure}, \link{plot_sensitivity}
}
