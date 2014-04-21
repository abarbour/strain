\name{calibrate}
\alias{calibrate}
\alias{calibrate.default}
\alias{pinv}
\title{Apply a calibration to strain data}
\usage{
calibrate(B, Cmat, invert = FALSE)

\method{calibrate}{default}(B, Cmat, invert = FALSE)

pinv(Cmat, ...)
}
\arguments{
  \item{B}{calibratee}

  \item{Cmat}{calibrator}

  \item{invert}{logical; should \code{Cmat} be (pseudo)
  inverted?}

  \item{...}{additional objects}
}
\description{
Apply a calibration to strain data
}

