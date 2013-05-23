\name{as.bsm}
\alias{as.bsm}
\alias{bsm}
\alias{gaugeOrientations.bsm}
\alias{get_az.bsm}
\alias{get_itype.bsm}
\alias{get_station.bsm}
\alias{is.bsm}
\alias{plot_orientations.bsm}
\alias{print.bsm}
\alias{rotate.bsm}
\title{Methods for borehole strainmeter (bsm) data}
\usage{
  as.bsm(X, g0az = 0, station = "", i.type = "GTSM", ...)

  is.bsm(X)

  print.bsm(B, ...)

  \method{gaugeOrientations}{bsm} (B,
    restrict.range = TRUE)

  \method{plot_orientations}{bsm} (B, opar = TRUE, ...)

  \method{get_itype}{bsm} (B)

  \method{get_station}{bsm} (B)

  \method{get_itype}{bsm} (B)

  \method{rotate}{bsm} (B)
}
\arguments{
  \item{X}{generic object}

  \item{g0az}{numeric; the orientation of the zeroth gauge}

  \item{station}{character; name of the station associated
  with the data}

  \item{i.type}{character; instrument type}

  \item{B}{object of class \code{bsm}}

  \item{...}{additional arguments}
}
\description{
  Methods for borehole strainmeter (bsm) data
}

