\docType{methods}
\name{bsm-methods}
\alias{bsm-methods}
\alias{bsm_methods}
\alias{calibMatrix.bsm}
\alias{calibrate.bsm}
\alias{extensions.bsm}
\alias{gaugeOrientations.bsm}
\alias{get_az.bsm}
\alias{get_itype.bsm}
\alias{get_station.bsm}
\alias{is.calibrated.bsm}
\alias{plot_orientations.bsm}
\alias{principals.bsm}
\alias{print.bsm}
\alias{rotate.bsm}
\alias{strains.bsm}
\title{Methods for borehole strainmeter (bsm) data}
\usage{
  print.bsm(B, ...)

  \method{gaugeOrientations}{bsm} (B,
    restrict.range = TRUE)

  \method{plot_orientations}{bsm} (B, ...)

  \method{is.calibrated}{bsm} (B)

  \method{calibrate}{bsm} (B, ...)

  \method{calibMatrix}{bsm} (B, ...)

  \method{get_az}{bsm} (B)

  \method{get_station}{bsm} (B)

  \method{get_itype}{bsm} (B)

  \method{rotate}{bsm} (B)

  \method{strains}{bsm} (B,
    strn.type = c("gauge", "calib"), ...)

  \method{principals}{bsm} (B, ...)

  \method{extensions}{bsm} (B, ...)
}
\arguments{
  \item{B}{object of class \code{bsm}}

  \item{...}{additional arguments}

  \item{restrict.range}{logical; should the orientiations
  be plotted in appropriate uniaxial directions?}

  \item{strn.type}{character; the type of strain to
  retrieve}
}
\description{
  Methods for borehole strainmeter (bsm) data
}
\author{
  A.J. Barbour <andy.barbour@gmail.com>
}

