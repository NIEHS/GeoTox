% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_resp.R
\name{plot_resp}
\alias{plot_resp}
\title{Plot GeoTox reponse data}
\usage{
plot_resp(
  resp,
  region_boundary,
  group_boundary = NULL,
  metric = c("GCA.Eff", "IA.Eff", "GCA.HQ.10", "IA.HQ.10"),
  quantiles = c(0.5),
  quantile_labels = c("Median")
)
}
\arguments{
\item{resp}{.}

\item{region_boundary}{.}

\item{group_boundary}{.}

\item{metric}{response metric, one of "GCA.Eff", "IA.Eff", "GCA.HQ.10"
or "IA.HQ.10"}

\item{quantiles}{quantiles to plot}

\item{quantile_labels}{labels for quantiles}
}
\value{
ggplot2 figure object
}
\description{
Plot GeoTox reponse data
}
