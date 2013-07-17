\name{calmat}
\alias{calmat}
\alias{calmat_reshape}
\alias{calmat_reshape.default}
\alias{calmat.cal.pbou}
\alias{calmat.cal.surf1}
\alias{calmat.default}
\title{Extract a calibration matrix from an object}
\usage{
  calmat(X, station, ...)

  \method{calmat}{default} (X, station, ...)

  \method{calmat}{cal.pbou} (X, station, ...)

  \method{calmat}{cal.surf1} (X, station, ...)

  calmat_reshape(X, ...)

  \method{calmat_reshape}{default} (X, inv.type = NULL,
    coeff.names = c("S11", "S21", "S31", "S41", "S12", "S22", "S32", "S42", "S13", "S23", "S33", "S43"),
    ...)
}
\arguments{
  \item{X}{calibration table}

  \item{station}{}

  \item{inv.type}{}

  \item{coeff.names}{}

  \item{...}{additional parameters}
}
\description{
  Extract a calibration matrix from an object
}

