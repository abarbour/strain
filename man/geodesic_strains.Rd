\name{geodesic_strains}
\alias{geodesic_strains}
\alias{geodesic_strains.default}
\title{Transform tensor strains into geodesic reference frame (radial, transverse)}
\usage{
geodesic_strains(E11, E22, E12, geod.deg = 0, ...)

\method{geodesic_strains}{default}(E11, E22, E12, geod.deg = 0, ...)
}
\arguments{
  \item{E11}{numeric; 1-axis strain}

  \item{E22}{numeric; 2-axis strain}

  \item{E12}{numeric; Shear strains}

  \item{theta}{numeric; the azimuth of the geodesic in the
  1-2 axis system}

  \item{...}{additional parameters passed to
  \code{\link{invariants}}}
}
\description{
Transform tensor strains into geodesic reference frame
(radial, transverse)
}
\seealso{
\code{\link{invariants}}
}

