\name{calmat}
\alias{calmat}
\alias{calmat.cal.pbou}
\alias{calmat.cal.surf1}
\alias{calmat.default}
\alias{calmat_reshape}
\alias{calmat_reshape.default}
\title{Extract a calibration matrix from an object}
\usage{
  calmat(Caltbl, station, ...)

  \method{calmat}{default} (Caltbl, station, ...)

  \method{calmat}{cal.pbou} (Caltbl, station, ...)

  \method{calmat}{cal.surf1} (Caltbl, station, ...)

  calmat_reshape(X, ...)

  \method{calmat_reshape}{default} (X, inv.type = NULL,
    coeff.names = c("S11", "S21", "S31", "S41", "S12", "S22", "S32", "S42", "S13", "S23", "S33", "S43"),
    ...)
}
\description{
  Extract a calibration matrix from an object
}

