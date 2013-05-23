\name{calibMatrix}
\alias{calibMatrix}
\alias{calibMatrix.default}
\title{Return a calibration matrix, inverted or not}
\usage{
  calibMatrix(C. = 1:4, D. = C., H. = C., axis_deg = 0,
    delta_deg = 0, pinvert = FALSE, verbose = TRUE, ...)

  calibMatrix.default(C. = 1:4, D. = C., H. = C.,
    axis_deg = 0, delta_deg = 0, pinvert = FALSE,
    verbose = TRUE, ...)
}
\arguments{
  \item{C.}{numeric;}

  \item{D.}{numeric;}

  \item{H.}{numeric;}

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
  Coefficients are in the form presented by Roeloffs 2010.

  The pseudo-inverse is computed using
  \code{corpcor::pseudoinverse} with default tolerances.
}

