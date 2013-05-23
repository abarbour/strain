\name{plot_orientations}
\alias{plot_orientations}
\alias{plot_orientations.default}
\title{Plot the orientations of the instrument gauges.}
\usage{
  plot_orientations(angs, name = "", opar = TRUE, ...)

  \method{plot_orientations}{default} (angs,
    gauge.labels = seq_along(angs), name = "", opar = TRUE,
    ...)
}
\arguments{
  \item{angs}{numeric; angles of gauges to plot}

  \item{name}{character; add something to the title}

  \item{opar}{logical; should the original graphics
  parameter be set upon exit?}

  \item{...}{additional parameters}
}
\description{
  Plot the orientations of the instrument gauges.
}

