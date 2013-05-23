\name{bsm-methods}
\alias{bsm_methods}
\alias{bsm-methods}
\alias{calibMatrix.bsm}
\alias{gaugeOrientations.bsm}
\alias{get_az.bsm}
\alias{get_itype.bsm}
\alias{get_rawstrain.bsm}
\alias{get_station.bsm}
\alias{is.calibrated.bsm}
\alias{plot_orientations.bsm}
\alias{principals.bsm}
\alias{print.bsm}
\alias{rotate.bsm}
\title{Methods for borehole strainmeter (bsm) data}
\usage{
  print.bsm(B, ...)

  \method{is.calibrated}{bsm} (X)

  \method{gaugeOrientations}{bsm} (B,
    restrict.range = TRUE)

  \method{plot_orientations}{bsm} (B, ...)

  \method{get_rawstrain}{bsm} (B)

  \method{get_az}{bsm} (B)

  \method{get_station}{bsm} (B)

  \method{get_itype}{bsm} (B)

  \method{rotate}{bsm} (B)

  \method{calibMatrix}{bsm} (C, ...)

  \method{principals}{bsm} (B)
}
\arguments{
  \item{X}{generic object}

  \item{g0az}{numeric; the orientation of the zeroth gauge}

  \item{station}{character; name of the station associated
  with the data}

  \item{i.type}{character; instrument type}

  \item{linearize}{logical; Should \code{X} be linearized?}

  \item{B}{object of class \code{bsm}}

  \item{...}{additional arguments}
}
\description{
  Methods for borehole strainmeter (bsm) data
}

