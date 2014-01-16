\name{geod_rotate}
\alias{geod_rotate}
\alias{geod_rotate.default}
\alias{geod_rotate2}
\title{Transform tensor strains in a 1-2 system into an r-t system}
\usage{
geod_rotate(E11, E22, E12, geodesic.deg = 0, NsEwNw = FALSE)

geod_rotate2(BSM, geodesic.deg = 0, ...)

\method{geod_rotate}{default}(E11, E22, E12, geodesic.deg = 0,
  NsEwNw = FALSE)
}
\arguments{
  \item{BSM}{numeric matrix with columns
  \code{c("ar","gam1","gam2")}}

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
\details{
\code{\link{geod_rotate2}} is a kludge for the moment, to
prove tensor strain rotations
}

