\name{extensions}
\alias{extensions}
\alias{extensions.default}
\title{Calculate extensions along specified axes}
\usage{
  extensions(S, ...)

  \method{extensions}{default} (S,
    orientations = c(0, 45, 90))
}
\arguments{
  \item{S}{calibrated strain data}

  \item{orientations}{numeric; the angles to evaluate
  extensional strain at}

  \item{...}{additional parameters}
}
\description{
  Calculate extensions along specified axes
}
\details{
  \code{orientations} is right-hand rule (ccw) relative the
  1-axis (x); in a compass system 0 would be to the West,
  and 90 to the North.
}

