% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stresses.R
\name{CoulombStress}
\alias{CoulombStress}
\alias{projection_matrix}
\title{Calculate Coulomb stresses on a plane from a given stress tensor}
\usage{
CoulombStress(S, n = NULL, mu = 0.6, B = 0.85, in.bar = FALSE, unit.scale = 1)

projection_matrix(n = c(1, 1, 1))
}
\arguments{
\item{S}{symmetric stress tensor, in Pascals}

\item{n}{an optional vector normal to the plane on which to project
the stresses... not used}

\item{mu}{coefficient of internal friction; Byerlee
puts this at 0.6 for deep rock}

\item{B}{Skempton's coefficient for an undrained material;
0.85 is a reasonable number for bulk Westerly granite}

\item{in.bar}{logical; should the stresses be in units of bar rather than Pa?}

\item{unit.scale}{numeric; stresses will be multiplied by this value.
Useful to return, say, gigaPascals (use \code{unit.scale=1e-9} then); but, note
that because Pore pressure is derived from volume stress, that too will be
scaled by this value.}
}
\description{
Calculate Coulomb stresses on a plane from a given stress tensor
}
\seealso{
\code{\link{stresses}}
}
