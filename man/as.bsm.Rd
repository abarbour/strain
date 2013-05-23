\name{as.bsm}
\alias{as.bsm}
\alias{as.bsm.default}
\alias{bsm}
\alias{is.bsm}
\alias{is.calibrated_strain}
\alias{is.raw_strain}
\title{Convert an object to one with class \code{'bsm'}}
\usage{
  as.bsm(X, ...)

  \method{as.bsm}{default} (X, g0az = 0, station = "",
    i.type = "GTSM", linearize = TRUE, ...)

  is.bsm(X)

  is.raw_strain(X)

  is.calibrated_strain(X)
}
\arguments{
  \item{X}{generic object}

  \item{g0az}{numeric; the orientation of the zeroth gauge}

  \item{station}{character; name of the station associated
  with the data}

  \item{i.type}{character; instrument type}

  \item{linearize}{logical; Should \code{X} be linearized?}

  \item{...}{additional arguments}
}
\description{
  Convert an object to one with class \code{'bsm'}
}

