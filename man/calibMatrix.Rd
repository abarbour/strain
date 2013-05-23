\name{calibMatrix}
\alias{calibMatrix}
\alias{calibMatrix.default}
\title{Return a calibration matrix, inverted or not}
\usage{
  calibMatrix(C, ...)

  \method{calibMatrix}{default} (C, D = NULL, H = NULL,
    axis_deg = 0, delta_deg = 0, invert = FALSE,
    verbose = TRUE, ...)
}
\arguments{
  \item{C}{numeric;}

  \item{D}{numeric;}

  \item{H}{numeric;}

  \item{axis_deg}{numeric;}

  \item{delta_deg}{numeric;}

  \item{pinvert}{logical; should the matrix be
  (pseudo-)inverted?}

  \item{...}{additional parameters}
}
\description{
  Return a calibration matrix, inverted or not
}
\details{
  The argument \code{C} may be a matrix 4with three columns
  representing \code{C}, \code{D}, and \code{H}
  respectively.  If that's the case, any arguments given
  for \code{D}, and \code{H} are ignored.

  Coefficients are in the form presented by Roeloffs 2010.

  The pseudo-inverse is computed using
  \code{corpcor::pseudoinverse} with default tolerances.
}

