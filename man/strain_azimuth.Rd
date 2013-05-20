\name{strain_azimuth}
\alias{.az_wrap}
\alias{strain_azimuth}
\title{Map an angle into the corresponding strain angle.}
\usage{
  strain_azimuth(Az)

  .az_wrap(Az, wraps = c(360))
}
\arguments{
  \item{Az}{numeric; an angle, in degrees}

  \item{wraps}{numeric; the bounding range to map angles
  into.}
}
\description{
  Uniaxial strains in parallel directions are
  indistinguishible; this function maps an angle into a
  physically realistic range: \eqn{(-90,90]}
}
\details{
  The mapping is relative to the zero-angle.  The mapping
  uses \code{\link{.az_wrap}} with \code{wraps=180}.
}

