\name{principals}
\alias{principals}
\alias{principals.lsm2}
\alias{principals.lsm3}
\title{Calculate principal strains}
\usage{
  principals(S, ...)

  principals.lsm3(L)

  principals.lsm2(L)
}
\arguments{
  \item{S}{a strain object}

  \item{...}{additional parameters}
}
\description{
  Strainmeters are generally designed to resemble common
  strain-gauge rosette configurations to simplify gauge
  combinations.
}
\details{
  If there are only two components of the LSM the angle of
  maximum shear cannot be solved for.
}
\section{Rosette classifications}{
  \itemize{ \item{BSM}{T-Delta} \item{LSM-3}{Rectangular
  three-gauge \eqn{45^{\circ}}, star shaped}
  \item{LSM-2}{Rectangular two-gauge \eqn{90^{\circ}}} }
}
\references{
  Altamani 19XX, "Strain Rosettes" by J.H.Meir, p. 412
}

