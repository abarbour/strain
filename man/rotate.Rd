\name{rotate}
\alias{rotate}
\alias{rotate.default}
\title{Apply a rotation of vectors in the 1-2 plane about the 3-axis}
\usage{
  rotate(X1, X2 = NULL, theta.deg = 0,
    hand.rule = c("right", "left"), ...)

  rotate.default(X1, X2 = NULL, theta.deg = 0,
    hand.rule = c("right", "left"), ...)
}
\arguments{
  \item{X1}{numeric; 1-axis components}

  \item{X2}{numeric; 2-axis components}

  \item{B}{object with class \code{'bsm'}}

  \item{theta.deg}{numeric; the rotation, in degrees, to
  apply}

  \item{hand.rule}{character; the sense of rotation}

  \item{...}{additional parameters}
}
\description{
  Apply a rotation of vectors in the 1-2 plane about the
  3-axis
}
\details{
  If \code{X2} is null the function assumes \code{X1} is a
  two column object, and that the first column is the
  1-axis information.

  \code{hand.rule} determines whether the rotation is
  clockwise ("left") or--the default--anti-clockwise
  ("right").
}

