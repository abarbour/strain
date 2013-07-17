\name{geod_rotate}
\alias{geod_rotate}
\alias{geod_rotate.default}
\title{Transform tensor strains in a 1-2 system into an r-t system}
\usage{
  geod_rotate(E11, E22, E12, geodesic.deg = 0,
    NsEwNw = FALSE)

  geod_rotate.default(E11, E22, E12, geodesic.deg = 0,
    NsEwNw = FALSE)
}
\arguments{
  \item{E11}{numeric; 1-axis strain}

  \item{E22}{numeric; 2-axis strain}

  \item{E12}{numeric; shear strain}

  \item{geodesic.deg}{numeric; the azimuth of the geodesic
  (in degrees)}

  \item{...}{additional parameters}
}
\description{
  \code{hand.rule} determines whether the rotation is
  clockwise ("left") or--the default--anti-clockwise
  ("right").
}

